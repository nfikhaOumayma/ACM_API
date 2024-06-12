/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import static com.acm.TestData.listOfPersons;
import static org.hamcrest.number.OrderingComparison.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

/**
 * Test case showing how a {@link Comparator} can be implemented using a lambda expression.
 *
 * @author HaythemBenizid
 * @since 0.1.1
 */
public class LambdaBasedComparatorTest {

	/** The persons. */
	private List<Person> persons;

	/**
	 * Sets the up.
	 *
	 * @throws Exception the exception
	 */
	@Before
	public void setUp() throws Exception {

		persons = listOfPersons();
	}

	/**
	 * Lambda based comparator.
	 *
	 * @throws Exception the exception
	 */
	@Test
	public void lambdaBasedComparator() throws Exception {

		/*
		 * comparator is defined on the fly
		 */
		Comparator<Person> byLastNameAsc = (p1, p2) -> p1.getLastName().compareTo(p2.getLastName());

		Collections.sort(persons, byLastNameAsc);

		for (int i = 0; i < persons.size(); i++) {
			Person current = persons.get(i);
			if (i < persons.size() - 1) {
				Person next = persons.get(i + 1);
				assertThat(current.getLastName().compareTo(next.getLastName()),
						lessThanOrEqualTo(0));
			}
		}
	}
}
