/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link MailBuilderMethod} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public enum MailBuilderMethod {

	/** The build generic Template. */
	BUILD_GENERIC("build"),

	// Method in class MailContentBuilder.class
	/** The build submit. */
	BUILD_SUBMIT("buildSubmitEmail"),

	/** The build review. */
	BUILD_REVIEW("buildCheckReviewEmail"),

	/** The build last level approval. */
	BUILD_LAST_LEVEL_APPROVAL("buildCheckLastLevelApprovalEmail"),

	/** The build last level reject. */
	BUILD_LAST_LEVEL_REJECT("buildCheckLevelsRejectEmail"),

	// Method in class MailContentBuilderClient.class
	/** The build reject Template. */
	BUILD_CUSTOMER_REJECT("buildLoanRejectedClientEmail"),

	/** The build reject Template. */
	BUILD_CUSTOMER_REJECT_FOR_CUSTOMER("buildLoanRejectedClientEmailForMailCustomer"),

	/** The build customer ask for review. */
	BUILD_CUSTOMER_ASK_FOR_REVIEW("buildInformCustomerAskForReviewEmail"),

	/** The build client loan assigned. */
	BUILD_CLIENT_LOAN_ASSIGNED("buildInformClientLoanAssignedEmail"),

	/** The build customer accept. */
	BUILD_CUSTOMER_ACCEPT("buildInformCustomerApprovedEmail"),

	/** The build customer decline. */
	BUILD_CUSTOMER_DECLINE("buildInformCustomerDeclineEmail"),

	/** The build customer application accepted. */
	BUILD_CUSTOMER_APPLICATION_ACCEPTED("buildInformCustomerAcceptEmail"),

	/** The build new customer. */
	BUILD_NEW_CUSTOMER("buildNewCustomerEmail"),

	/** The build customer reset pwd. */
	BUILD_CUSTOMER_RESET_PWD("buildInformCustomerResetPwd"),

	/** The build user resend login. */
	BUILD_USER_RESEND_LOGIN("buildInformUserResendLogin"),

	/** The build add user. */
	BUILD_ADD_USER("buildAddUserEmail"),

	/** The build user assigned meza card. */
	BUILD_USER_ASSIGNED_MEZA_CARD("buildInformUserAssignedMezaCardEmail");

	/** The method name. */
	private String methodName;

	/**
	 * Instantiates a new mail builder method.
	 *
	 * @param methodName the method name
	 */
	MailBuilderMethod(String methodName) {

		this.methodName = methodName;
	}

	/**
	 * Method name.
	 *
	 * @return the string
	 */
	public String methodName() {

		return methodName;
	}
}
