/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.WithAssertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link ACMValidationUtilsTest} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RunWith(SpringRunner.class)
class ACMValidationUtilsTest implements WithAssertions {

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
	}

	/**
	 * Should return true when null collection.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNullCollection() {

		// GIVEN
		Collection<?> list = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(list);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should true when empty collection.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenEmptyCollection() {

		// GIVEN
		Collection<?> list = Collections.EMPTY_LIST;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(list);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should true when empty collection.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenFilledCollection() {

		// GIVEN
		Collection<String> list = Collections.singletonList("list");
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(list);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when null map.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNullMap() {

		// GIVEN
		Map<?, ?> map = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(map);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when empty map.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenEmptyMap() {

		// GIVEN
		Map<?, ?> map = new HashMap<Object, Object>();
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(map);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when empty map.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenMap() {

		// GIVEN
		Map<String, String> map = new HashMap<String, String>();
		map.put("test", "test");
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(map);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when null string.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNullString() {

		// GIVEN
		String maybeNullOrEmpty = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when null string.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhennullString() {

		// GIVEN
		String maybeNullOrEmpty = "null";
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when empty string.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenEmtyString() {

		// GIVEN
		String maybeNullOrEmpty = "";
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when not null string.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenString() {

		// GIVEN
		String maybeNullOrEmpty = "test";
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return false when not empty table.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNotNullTable() {

		// GIVEN
		Object[] maybeNullOrEmpty = {"César Cielo", "Filho", 1, "21.30", false};
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when null table.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNullTable() {

		// GIVEN
		Object[] maybeNullOrEmpty = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when empty table.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenEmptyTable() {

		// GIVEN
		Object[] maybeNullOrEmpty = {};
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when null date.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhennullDate() {

		// GIVEN
		Date maybeNullOrEmpty = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when date.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNewDate() {

		// GIVEN
		Date maybeNullOrEmpty = new Date();
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when null Object.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhennullObject() {

		// GIVEN
		Object maybeNullOrEmpty = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when null Object.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNotNullObject() {

		// GIVEN
		Object maybeNullOrEmpty = new Object();
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when null list.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhennullList() {

		// GIVEN
		List<?> maybeNullOrEmpty = null;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when empty list.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenEmptyList() {

		// GIVEN
		List<?> maybeNullOrEmpty = Collections.EMPTY_LIST;
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when null Object.
	 */
	@Test
	void shouldSuccessVerifyisNullOrEmptyWhenNotEmptyList() {

		// GIVEN
		List<String> maybeNullOrEmpty = Collections.singletonList("test");
		// WHEN
		boolean result = ACMValidationUtils.isNullOrEmpty(maybeNullOrEmpty);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when isNumeric.
	 */
	@Test
	void shouldSuccessVerifyWhenIsNumeric() {

		// GIVEN
		String string = "2";
		// WHEN
		boolean result = ACMValidationUtils.isNumeric(string);
		// THEN
		Assertions.assertThat(result).isTrue();
	}

	/**
	 * Should return true when isNumeric.
	 */
	@Test
	void shouldSuccessVerifyWhenIsNotNumeric() {

		// GIVEN
		String string = "hdy";
		// WHEN
		boolean result = ACMValidationUtils.isNumeric(string);
		// THEN
		Assertions.assertThat(result).isFalse();
	}

	/**
	 * Should return true when null string.
	 */
	@Test
	void shouldSuccessVerifyWhenIsNullString() {

		// GIVEN
		String string = null;
		// WHEN
		String result = ACMValidationUtils.notNullString(string);
		// THEN
		Assertions.assertThat(result).isEqualTo("");
	}

	/**
	 * Should return true when null string.
	 */
	@Test
	void shouldSuccessVerifyWhenIsNulllString() {

		// GIVEN
		String string = "null";
		// WHEN
		String result = ACMValidationUtils.notNullString(string);
		// THEN
		Assertions.assertThat(result).isEqualTo("");
	}

	/**
	 * Should return true when null string.
	 */
	@Test
	void shouldSuccessVerifyWhenIsNotNullString() {

		// GIVEN
		String string = " test ";
		// WHEN
		String result = ACMValidationUtils.notNullString(string);
		// THEN
		Assertions.assertThat(result).isEqualTo("test");
	}

	/**
	 * Should return true when null string.
	 */
	@Test
	void shouldSuccessCountNumberFromString() {

		// GIVEN
		String string = null;
		// WHEN
		Integer result = ACMValidationUtils.countNumberFromString(string);
		// THEN
		Assertions.assertThat(result).isEqualTo(0);
	}

	/**
	 * Should return decimal number.
	 */
	@Test
	void shouldSuccessGetDecimalNumber() {

		// GIVEN
		double number = 2d;
		int decimalNumber = 2;
		// WHEN
		double result = ACMValidationUtils.decimalFormatter(number, decimalNumber);
		// THEN
		Assertions.assertThat(result).isEqualTo(2.0);
	}

	/**
	 * Should return empty string when empty INput.
	 */
	@Test
	void shouldSuccessReturnEmptyStringWhenemptyIn() {

		// GIVEN
		String name = "";
		String firstName = "";
		// WHEN
		String result = ACMValidationUtils.formatFullName(name, firstName);
		// THEN
		Assertions.assertThat(result).isEqualTo("");
	}

	/**
	 * Should return empty string when null INput.
	 */
	@Test
	void shouldSuccessReturnEmptyStringWhenNullIn() {

		// GIVEN
		String name = null;
		String firstName = null;
		// WHEN
		String result = ACMValidationUtils.formatFullName(name, firstName);
		// THEN
		Assertions.assertThat(result).isEqualTo("");
	}

	/**
	 * Should return formated name.
	 */
	@Test
	void shouldSuccessReturnFormatedName() {

		// GIVEN
		String name = "name";
		String firstName = "first name";
		// WHEN
		String result = ACMValidationUtils.formatFullName(name, firstName);
		// THEN
		Assertions.assertThat(result).isEqualTo("name, first name");
	}

	/**
	 * Should return formated name.
	 */
	@Test
	void shouldSuccessReturnEmptyStringWhenFormatLabel() {

		// GIVEN
		String label = null;
		// WHEN
		String result = ACMValidationUtils.formatLabel(label);
		// THEN
		Assertions.assertThat(result).isEqualTo("");
	}

	/**
	 * Should return formated name.
	 */
	@Test
	void shouldSuccessReturntrimmedStringWhenFormatLabel() {

		// GIVEN
		String label = " test ";
		// WHEN
		String result = ACMValidationUtils.formatLabel(label);
		// THEN
		Assertions.assertThat(result).isEqualTo("test");
	}

}
