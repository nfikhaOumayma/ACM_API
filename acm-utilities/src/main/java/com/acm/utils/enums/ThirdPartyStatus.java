/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link ThirdPartyStatus} enum.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
public enum ThirdPartyStatus {

	/** The failed => KYC | AML | I-SCORE. */
	FAILED,

	/** The Error => AML | I-SCORE. */
	ERROR,

	/** The success => KYC | I-SCORE. */
	SUCCESS,

	/** The rejected => I-SCORE. */
	REJECTED,

	/** The rejecte guarantor => I-SCORE. */
	REJECTE_GUARANTOR,

	/** The accepted => AML| I-SCORE. */
	ACCEPTED,

	/** The refered => AML. */
	REFERED,

	/** The declined => AML. */
	DECLINED,

	/** The review I-SCORE. */
	REVIEW,

	/** The REVIEW guarantor => I-SCORE. */
	REVIEW_GUARANTOR,
}
