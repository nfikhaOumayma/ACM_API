/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.constants.common;

import java.util.Arrays;
import java.util.List;

/**
 * The {@link CommonConstantsIncentive} Class.
 * 
 * @author HaythemBenizid
 * @since 1.0.8
 */
public final class CommonConstantsIncentive implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1243592683339497161L;

	/**
	 * The Constant INCENTIVE_SETTING_CONSTANT_CATEGORY : ACM_INCENTIVE_REPAYMENT /
	 * ACM_INCENTIVE_REGESTRATION / ACM_INCENTIVE_OPERATION / ACM_INCENTIVE_LEGAL / FREQUENCY /
	 * INCENTIVE_SETTING_TYPE / INCENTIVE_REGESTRATION_CUSTOMER_TYPE.
	 */
	public static final List<String> INCENTIVE_SETTING_CONSTANT_CATEGORY =
			Arrays.asList("ACM_INCENTIVE_REPAYMENT", "ACM_INCENTIVE_REGESTRATION",
					"ACM_INCENTIVE_OPERATION", "ACM_INCENTIVE_LEGAL", "FREQUENCY",
					"INCENTIVE_SETTING_TYPE", "INCENTIVE_REGESTRATION_CUSTOMER_TYPE");

	/**
	 * The Constant INCENTIVE_SETTING_CATEGORY : ACTIVE_CUSTOMER / PRODUCTIVITY / RISK_LEVEL /
	 * DISCOUNT_FROM_TOTAL.
	 */
	public static final List<String> INCENTIVE_SETTING_CATEGORY =
			Arrays.asList("ACTIVE_CUSTOMER", "PRODUCTIVITY", "RISK_LEVEL", "DISCOUNT_FROM_TOTAL");

}
