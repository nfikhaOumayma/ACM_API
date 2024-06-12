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
 * QProductCategory is a Querydsl query type for ProductCategory.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QProductCategory extends EntityPathBase<ProductCategory> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1922908522L;

	/** The Constant productCategory. */
	public static final QProductCategory productCategory = new QProductCategory("productCategory");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code. */
	public final StringPath code = createString("code");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The incentive legal. */
	public final BooleanPath incentiveLegal = createBoolean("incentiveLegal");

	/** The incentive operation. */
	public final BooleanPath incentiveOperation = createBoolean("incentiveOperation");

	/** The incentive registration. */
	public final BooleanPath incentiveRegistration = createBoolean("incentiveRegistration");

	/** The incentive repayment. */
	public final BooleanPath incentiveRepayment = createBoolean("incentiveRepayment");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The product ids list. */
	public final StringPath productIdsList = createString("productIdsList");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q product category.
	 *
	 * @param variable the variable
	 */
	public QProductCategory(String variable) {

		super(ProductCategory.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q product category.
	 *
	 * @param path the path
	 */
	public QProductCategory(Path<? extends ProductCategory> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q product category.
	 *
	 * @param metadata the metadata
	 */
	public QProductCategory(PathMetadata metadata) {

		super(ProductCategory.class, metadata);
	}

}
