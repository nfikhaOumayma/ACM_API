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
 * QSettingMotifRejets is a Querydsl query type for SettingMotifRejets.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingMotifRejets extends EntityPathBase<SettingMotifRejets> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 456464803L;

	/** The Constant settingMotifRejets. */
	public static final QSettingMotifRejets settingMotifRejets =
			new QSettingMotifRejets("settingMotifRejets");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The categorie. */
	public final StringPath categorie = createString("categorie");

	/** The code. */
	public final StringPath code = createString("code");

	/** The code external. */
	public final NumberPath<Integer> codeExternal = createNumber("codeExternal", Integer.class);

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

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting motif rejets.
	 *
	 * @param variable the variable
	 */
	public QSettingMotifRejets(String variable) {

		super(SettingMotifRejets.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting motif rejets.
	 *
	 * @param path the path
	 */
	public QSettingMotifRejets(Path<? extends SettingMotifRejets> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting motif rejets.
	 *
	 * @param metadata the metadata
	 */
	public QSettingMotifRejets(PathMetadata metadata) {

		super(SettingMotifRejets.class, metadata);
	}

}
