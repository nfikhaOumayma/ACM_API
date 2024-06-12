/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import java.time.LocalDate;
import java.time.Period;

/**
 * A simple class that represents a person.
 *
 * @author HaythemBenizid
 * @since 0.1.1
 */
public class Person {

	/**
	 * The Enum Gender.
	 */
	public enum Gender {

		/** The male. */
		MALE,
		/** The female. */
		FEMALE
	}

	/** The first name. */
	private final String firstName;

	/** The last name. */
	private final String lastName;

	/** The birth day. */
	private final LocalDate birthDay;

	/** The gender. */
	private Gender gender;

	/**
	 * Instantiates a new person.
	 *
	 * @param firstname the firstname
	 * @param lastName the last name
	 * @param birthDay the birth day
	 * @param gender the gender
	 */
	public Person(String firstname, String lastName, LocalDate birthDay, Gender gender) {

		this.firstName = firstname;
		this.lastName = lastName;
		this.birthDay = birthDay;
		this.gender = gender;
	}

	/**
	 * Gets the first name.
	 *
	 * @return the first name
	 */
	public String getFirstName() {

		return firstName;
	}

	/**
	 * Gets the last name.
	 *
	 * @return the last name
	 */
	public String getLastName() {

		return lastName;
	}

	/**
	 * Gets the birth day.
	 *
	 * @return the birth day
	 */
	public LocalDate getBirthDay() {

		return birthDay;
	}

	/**
	 * Gets the gender.
	 *
	 * @return the gender
	 */
	public Gender getGender() {

		return gender;
	}

	/**
	 * Sets the gender.
	 *
	 * @param gender the new gender
	 */
	public void setGender(Gender gender) {

		this.gender = gender;
	}

	/**
	 * Gets the age.
	 *
	 * @return the age
	 */
	public int getAge() {

		return Period.between(getBirthDay(), LocalDate.now()).getYears();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {

		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}

		Person person = (Person) o;

		if (birthDay != null ? !birthDay.equals(person.birthDay) : person.birthDay != null) {
			return false;
		}
		if (gender != person.gender) {
			return false;
		}
		if (lastName != null ? !lastName.equals(person.lastName) : person.lastName != null) {
			return false;
		}
		if (firstName != null ? !firstName.equals(person.firstName) : person.firstName != null) {
			return false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {

		int result = firstName != null ? firstName.hashCode() : 0;
		result = 31 * result + (lastName != null ? lastName.hashCode() : 0);
		result = 31 * result + (birthDay != null ? birthDay.hashCode() : 0);
		result = 31 * result + (gender != null ? gender.hashCode() : 0);
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "Person{" + "firstName='" + firstName + '\'' + ", lastName='" + lastName + '\''
				+ ", birthDay=" + birthDay + ", gender=" + gender + '}';
	}
}
