- in difference to the old system we will primarily use id's
  in collisions.
- collisions will be stored in attributes (for uniformity)
  already there there's an improvement to the old engine.
- Where is collisionType stored?
  - on attributes, the will prolly be the standard answer
    because it's the most ehm general.
- When does detect_collisions run?
  - where did it run in the old engine, before object
    ticking is my bet. sounds reasonable.
- Clear collision_ids each frame?
  - yes
- wait_until(collision_id)
  collision_id does not exist at all until a collision
  it is removed after ehm every frame, so survives 1 frame.