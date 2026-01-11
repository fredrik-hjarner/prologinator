import {
    ANSI, clearScreen, waitForKey
} from "./terminal.ts";

const COLUMN_WIDTH = 60;   // width of one column
const COLUMN_GAP = 2;      // spaces between columns

export interface SelectableItem {
    id: string;      // The actual value (path or pattern)
    label: string;   // What to show on screen
    selected: boolean;
}

interface SelectorOptions {
    allowReorder?: boolean;
    showCounts?: boolean;
}

export class InteractiveSelector {
    private items: SelectableItem[];
    private currentIndex = 0;
    private title: string;
    private options: SelectorOptions;

    constructor(
        items: SelectableItem[],
        title: string,
        options: SelectorOptions = {}
    ) {
        this.items = items;
        this.title = title;
        this.options = {
            allowReorder: true,
            showCounts: true,
            ...options
        };
    }

    private renderItem(idx: number): string {
        if (idx >= this.items.length) {
            return ''.padEnd(COLUMN_WIDTH);
        }
    
        const item = this.items[idx];
        const isCurrent = idx === this.currentIndex;
        const marker = item.selected ? '✓' : ' ';
        const cursor = isCurrent ? '▶' : ' ';
    
        let line = `${cursor} [${marker}] ${item.label}`;
    
        // Hard truncate to column width
        if (line.length > COLUMN_WIDTH) {
            line = line.slice(0, COLUMN_WIDTH - 1) + '…';
        }
    
        line = line.padEnd(COLUMN_WIDTH);
    
        if (isCurrent) {
            line = `${ANSI.REVERSE}${line}${ANSI.RESET}`;
        }
    
        return line;
    }

    private renderItems() {
        const rows = Math.ceil(this.items.length / 2);
    
        for (let row = 0; row < rows; row++) {
            const leftIdx = row;
            const rightIdx = row + rows;
    
            const left = this.renderItem(leftIdx);
            const right = this.renderItem(rightIdx);
    
            console.log(left + ' '.repeat(COLUMN_GAP) + right);
        }
    }    

    private render() {
        clearScreen();
        // Hide cursor during render to prevent flickering
        process.stdout.write(ANSI.HIDE_CURSOR);
        
        const selectedCount = this.items.filter(i => i.selected).length;
        
        console.log('╔═══════════════════════════════════════════════════════════╗');
        console.log(`║  ${this.title.padEnd(56)} ║`);
        console.log('╚═══════════════════════════════════════════════════════════╝\n');
        console.log(`Files: ${this.items.length} total, ${selectedCount} selected\n`);
        console.log('Controls:');
        console.log('  ↑/↓ - Navigate');
        console.log('  Space - Toggle selection');
        console.log('  a - Select all');
        console.log('  n - Unselect all');
        if (this.options.allowReorder) {
            console.log('  w - Move item up');
            console.log('  s - Move item down');
        }        
        console.log('  Enter - Confirm and generate output');
        console.log('  q - Quit\n');
        console.log('─'.repeat(60));

        this.renderItems();
        console.log('─'.repeat(COLUMN_WIDTH * 2 + COLUMN_GAP));
    }

    public async run(): Promise<string[]> {
        while (true) {
            this.render();
            const input = await waitForKey();
            const char = input.toLowerCase();

            // Navigation
            if (input === ANSI.Up) {
                this.currentIndex = Math.max(0, this.currentIndex - 1);
            } else if (input === ANSI.Down) {
                this.currentIndex = Math.min(this.items.length - 1, this.currentIndex + 1);
            } 
            // Actions
            else if (input === ' ') {
                this.items[this.currentIndex].selected = !this.items[this.currentIndex].selected;
            } else if (char === 'a') {
                this.items.forEach(i => i.selected = true);
            } else if (char === 'n') {
                this.items.forEach(i => i.selected = false);
            } else if (this.options.allowReorder &&char === 'w' && this.currentIndex > 0) {
                // Move item up (Reorder)
                const cur = this.items[this.currentIndex];
                const prev = this.items[this.currentIndex - 1];
                this.items[this.currentIndex] = prev;
                this.items[this.currentIndex - 1] = cur;
                this.currentIndex--;
            } else if (this.options.allowReorder &&char === 's' && this.currentIndex < this.items.length - 1) {
                // Move item down (Reorder)
                const cur = this.items[this.currentIndex];
                const next = this.items[this.currentIndex + 1];
                this.items[this.currentIndex] = next;
                this.items[this.currentIndex + 1] = cur;
                this.currentIndex++;
            }
            // Control
            else if (input === '\r' || input === '\n') {
                break;
            } else if (char === 'q' || input === '\x03') { // q or Ctrl+C
                process.exit(0);
            }
        }

        return this.items.filter(i => i.selected).map(i => i.id);
    }
}
