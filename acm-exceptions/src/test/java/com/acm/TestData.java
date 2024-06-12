/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

/**
 * Utility class that generates some test data for us.
 *
 * @author HaythemBenizid
 * @since 0.1.1
 */
public class TestData {

	/**
	 * List of persons.
	 *
	 * @return the list
	 */
	public static List<Person> listOfPersons() {

		return Arrays.asList(
				new Person("Jane", "Jungle", LocalDate.of(1978, 12, 15), Person.Gender.FEMALE),
				new Person("Mary", "Smith", LocalDate.of(1980, 10, 19), Person.Gender.FEMALE),
				new Person("John", "Dole", LocalDate.of(1973, 5, 31), Person.Gender.MALE),
				new Person("Maggie", "Simpson", LocalDate.of(2012, 10, 18), Person.Gender.FEMALE));
	}

}
