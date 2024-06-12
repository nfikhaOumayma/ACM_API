/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import java.util.Properties;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * {@link OperatingSystemTest} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class OperatingSystemTest {

	/**
	 * On linux or windows.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	@EnabledOnOs({OS.LINUX, OS.WINDOWS})
	void onLinuxOrWindows() {

		System.out.println("Run this on Linux or Windows!");
	}

	/**
	 * On windows.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	@EnabledOnOs({OS.WINDOWS})
	void onWindows() {

		System.out.println("Run this on Windows!");
		Properties properties = System.getProperties();
		properties.forEach((k, v) -> System.out.println(k + ":" + v));
	}

	/**
	 * On linux.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	@EnabledOnOs({OS.LINUX})
	void onLinux() {

		System.out.println("Run this on Linux!");
		Properties properties = System.getProperties();
		properties.forEach((k, v) -> System.out.println(k + ":" + v));
	}

	/**
	 * Not on windows.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	@DisabledOnOs({OS.WINDOWS, OS.SOLARIS, OS.MAC})
	void notOnWindows() {

		System.out.println("Do not run this on Windows,  Solaris or MAC!");
	}

}
