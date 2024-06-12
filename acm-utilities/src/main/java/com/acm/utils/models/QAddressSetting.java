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
 * QAddressSetting is a Querydsl query type for AddressSetting.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAddressSetting extends EntityPathBase<AddressSetting> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -164517101L;

	/** The Constant addressSetting. */
	public static final QAddressSetting addressSetting = new QAddressSetting("addressSetting");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The address list id. */
	public final NumberPath<Long> addressListId = createNumber("addressListId", Long.class);

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

	/** The id extern. */
	public final StringPath idExtern = createString("idExtern");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The parent id. */
	public final NumberPath<Long> parentId = createNumber("parentId", Long.class);

	/** The table abacus name. */
	public final StringPath tableAbacusName = createString("tableAbacusName");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The value json. */
	public final StringPath valueJson = createString("valueJson");

	/**
	 * Instantiates a new q address setting.
	 *
	 * @param variable the variable
	 */
	public QAddressSetting(String variable) {

		super(AddressSetting.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q address setting.
	 *
	 * @param path the path
	 */
	public QAddressSetting(Path<? extends AddressSetting> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q address setting.
	 *
	 * @param metadata the metadata
	 */
	public QAddressSetting(PathMetadata metadata) {

		super(AddressSetting.class, metadata);
	}

}
