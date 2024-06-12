/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.constants.common;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * The Class CommonLicenceVariable.
 */
@Component
@Scope("prototype")
public final class CommonLicenceVariable implements java.io.Serializable {

	/** The max sumiltanious user. */
	public static long currentSumiltaniousUser;

	/** The max active user. */
	public static Integer maxActiveUser;

	/** The mac adress server. */
	public static String macAdressServer;

}
