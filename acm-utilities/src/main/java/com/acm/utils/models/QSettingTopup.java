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
 * QSettingTopup is a Querydsl query type for SettingTopup.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingTopup extends EntityPathBase<SettingTopup> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 65890135L;

	/** The Constant settingTopup. */
	public static final QSettingTopup settingTopup = new QSettingTopup("settingTopup");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The topup max allowed topups. */
	public final NumberPath<Integer> topupMaxAllowedTopups =
			createNumber("topupMaxAllowedTopups", Integer.class);

	/** The topup max continuous late days. */
	public final NumberPath<Integer> topupMaxContinuousLateDays =
			createNumber("topupMaxContinuousLateDays", Integer.class);

	/** The topup max separate late days. */
	public final NumberPath<Integer> topupMaxSeparateLateDays =
			createNumber("topupMaxSeparateLateDays", Integer.class);

	/** The topup min fixed loan amount. */
	public final NumberPath<java.math.BigDecimal> topupMinFixedLoanAmount =
			createNumber("topupMinFixedLoanAmount", java.math.BigDecimal.class);

	/** The topup min loan amount type. */
	public final NumberPath<Integer> topupMinLoanAmountType =
			createNumber("topupMinLoanAmountType", Integer.class);

	/** The topup min loan payment percentage. */
	public final NumberPath<Float> topupMinLoanPaymentPercentage =
			createNumber("topupMinLoanPaymentPercentage", Float.class);

	/** The topup min previously issued loans number. */
	public final NumberPath<Integer> topupMinPreviouslyIssuedLoansNumber =
			createNumber("topupMinPreviouslyIssuedLoansNumber", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting topup.
	 *
	 * @param variable the variable
	 */
	public QSettingTopup(String variable) {

		super(SettingTopup.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting topup.
	 *
	 * @param path the path
	 */
	public QSettingTopup(Path<? extends SettingTopup> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting topup.
	 *
	 * @param metadata the metadata
	 */
	public QSettingTopup(PathMetadata metadata) {

		super(SettingTopup.class, metadata);
	}

}
