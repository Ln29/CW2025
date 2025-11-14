package com.comp2042.logic.bricks;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Seven-bag brick generator that implements fair brick distribution.
 * Generates bricks in shuffled bags of 7, ensuring each brick type appears exactly once per bag.
 */
public class SevenBagBrickGenerator implements BrickGenerator {

    private final Deque<Brick> nextBricks = new ArrayDeque<>();

    /**
     * Constructor that initializes the generator with two bags of bricks.
     */
    public SevenBagBrickGenerator() {
        addNewBag();
        addNewBag();
    }

    /**
     * Gets the next brick from the queue.
     * Automatically refills the bag when running low on bricks.
     *
     * @return the next brick to be used
     */
    @Override
    public Brick getBrick() {
        // Add more bricks if we're running low
        if (nextBricks.size() < 7) {
            addNewBag();
        }
        return nextBricks.poll();
    }

    /**
     * Peeks at the next brick without removing it from the queue.
     *
     * @return the next brick that will be returned by getBrick()
     */
    @Override
    public Brick getNextBrick() {
        return nextBricks.peek();
    }

    /**
     * Gets the next N bricks without removing them from the queue.
     *
     * @param count the number of bricks to peek at
     * @return a list of the next N bricks
     */
    @Override
    public List<Brick> getNextBricks(int count) {
        List<Brick> result = new ArrayList<>();
        Iterator<Brick> iterator = nextBricks.iterator();
        int added = 0;
        while (iterator.hasNext() && added < count) {
            result.add(iterator.next());
            added++;
        }
        return result;
    }

    /**
     * Creates a new bag containing all 7 brick types in random order
     * and adds them to the queue.
     */
    private void addNewBag() {
        // Create a list with all 7 brick types
        List<Brick> bag = new ArrayList<>();
        bag.add(new IBrick());
        bag.add(new JBrick());
        bag.add(new LBrick());
        bag.add(new OBrick());
        bag.add(new SBrick());
        bag.add(new TBrick());
        bag.add(new ZBrick());

        // Shuffle the bag to randomize order
        shuffleBag(bag);

        // Add all shuffled bricks to the queue
        for (int i = 0; i < bag.size(); i++) {
            nextBricks.add(bag.get(i));
        }
    }

    /**
     * Shuffles the brick list using Fisher-Yates shuffle algorithm.
     *
     * @param bag the list of bricks to shuffle
     */
    private void shuffleBag(List<Brick> bag) {
        // Iterate backwards through the list
        for (int i = bag.size() - 1; i > 0; i--) {
            // Pick a random index from 0 to i
            int j = ThreadLocalRandom.current().nextInt(i + 1);
            // Swap elements at positions i and j
            Brick temp = bag.get(i);
            bag.set(i, bag.get(j));
            bag.set(j, temp);
        }
    }
}