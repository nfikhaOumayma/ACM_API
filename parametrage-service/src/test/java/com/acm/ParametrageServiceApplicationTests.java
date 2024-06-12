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
 * The {@link CrmServiceApplicationTests} Class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RunWith(SpringRunner.class)
@SpringBootTest
public class ParametrageServiceApplicationTests {

	/**
	 * Should run server when lunch main app.
	 */
	/* Test class added ONLY to cover main() invocation not covered by application tests. */
	@Test
	public void shouldRunServerWhenLunchMainApp() {

		ParametrageServiceApplication.main(new String[] {});
	}

}
