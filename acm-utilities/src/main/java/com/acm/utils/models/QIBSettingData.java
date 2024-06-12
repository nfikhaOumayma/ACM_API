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
 * QIBSettingData is a Querydsl query type for IBSettingData.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIBSettingData extends EntityPathBase<IBSettingData> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -43238102L;

	/** The Constant iBSettingData. */
	public static final QIBSettingData iBSettingData = new QIBSettingData("iBSettingData");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The branche localisation. */
	public final StringPath brancheLocalisation = createString("brancheLocalisation");

	/** The code. */
	public final StringPath code = createString("code");

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

	/** The parent id. */
	public final NumberPath<Long> parentId = createNumber("parentId", Long.class);

	/** The phone number. */
	public final StringPath phoneNumber = createString("phoneNumber");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The value. */
	public final StringPath value = createString("value");

	/**
	 * Instantiates a new QIB setting data.
	 *
	 * @param variable the variable
	 */
	public QIBSettingData(String variable) {

		super(IBSettingData.class, forVariable(variable));
	}

	/**
	 * Instantiates a new QIB setting data.
	 *
	 * @param path the path
	 */
	public QIBSettingData(Path<? extends IBSettingData> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new QIB setting data.
	 *
	 * @param metadata the metadata
	 */
	public QIBSettingData(PathMetadata metadata) {

		super(IBSettingData.class, metadata);
	}

}
