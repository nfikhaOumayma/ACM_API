/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The {@link GedServiceApplicationTests} Class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@RunWith(SpringRunner.class)
@SpringBootTest
public class GedServiceApplicationTests {

	/**
	 * Should run server when lunch main app.
	 */
	/* Test class added ONLY to cover main() invocation not covered by application tests. */
	@Test
	public void shouldRunServerWhenLunchMainApp() {

		GedServiceApplication.main(new String[] {});
	}

}
