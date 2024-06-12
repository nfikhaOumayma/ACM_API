/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QIncentiveRepaymentRun is a Querydsl query type for IncentiveRepaymentRun.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveRepaymentRun extends EntityPathBase<IncentiveRepaymentRun> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1414233364L;

	/** The Constant incentiveRepaymentRun. */
	public static final QIncentiveRepaymentRun incentiveRepaymentRun =
			new QIncentiveRepaymentRun("incentiveRepaymentRun");

	/** The active customer. */
	public final NumberPath<Long> activeCustomer = createNumber("activeCustomer", Long.class);

	/** The balance not paid. */
	public final NumberPath<Long> balanceNotPaid = createNumber("balanceNotPaid", Long.class);

	/** The balance paid. */
	public final NumberPath<Long> balancePaid = createNumber("balancePaid", Long.class);

	/** The based on. */
	public final StringPath basedOn = createString("basedOn");

	/** The branch. */
	public final StringPath branch = createString("branch");

	/** The discount. */
	public final BooleanPath discount = createBoolean("discount");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The incentive value. */
	public final NumberPath<Long> incentiveValue = createNumber("incentiveValue", Long.class);

	/** The incentive value setting. */
	public final NumberPath<Long> incentiveValueSetting =
			createNumber("incentiveValueSetting", Long.class);

	/** The issue loan month. */
	public final NumberPath<Long> issueLoanMonth = createNumber("issueLoanMonth", Long.class);

	/** The loan branch manger. */
	public final StringPath loanBranchManger = createString("loanBranchManger");

	/** The loan officer name. */
	public final StringPath loanOfficerName = createString("loanOfficerName");

	/** The loan supervisor. */
	public final StringPath loanSupervisor = createString("loanSupervisor");

	/** The month. */
	public final StringPath month = createString("month");

	/** The no discount value. */
	public final NumberPath<Long> noDiscountValue = createNumber("noDiscountValue", Long.class);

	/** The product category description. */
	public final StringPath productCategoryDescription = createString("productCategoryDescription");

	/** The product category id. */
	public final NumberPath<Long> productCategoryId = createNumber("productCategoryId", Long.class);

	/** The productivity. */
	public final BooleanPath productivity = createBoolean("productivity");

	/** The product list ids. */
	public final StringPath productListIds = createString("productListIds");

	/** The repayment not paid. */
	public final NumberPath<Long> repaymentNotPaid = createNumber("repaymentNotPaid", Long.class);

	/** The repayment paid. */
	public final NumberPath<Long> repaymentPaid = createNumber("repaymentPaid", Long.class);

	/** The repayment rate. */
	public final NumberPath<Long> repaymentRate = createNumber("repaymentRate", Long.class);

	/** The report name. */
	public final StringPath reportName = createString("reportName");

	/** The risk. */
	public final NumberPath<Float> risk = createNumber("risk", Float.class);

	/** The role. */
	public final StringPath role = createString("role");

	/** The run date. */
	public final DateTimePath<java.util.Date> runDate =
			createDateTime("runDate", java.util.Date.class);

	/** The total loan amount. */
	public final NumberPath<Long> totalLoanAmount = createNumber("totalLoanAmount", Long.class);

	/** The user name. */
	public final StringPath userName = createString("userName");

	/**
	 * Instantiates a new q incentive repayment run.
	 *
	 * @param variable the variable
	 */
	public QIncentiveRepaymentRun(String variable) {

		super(IncentiveRepaymentRun.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q incentive repayment run.
	 *
	 * @param path the path
	 */
	public QIncentiveRepaymentRun(Path<? extends IncentiveRepaymentRun> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q incentive repayment run.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveRepaymentRun(PathMetadata metadata) {

		super(IncentiveRepaymentRun.class, metadata);
	}

}
