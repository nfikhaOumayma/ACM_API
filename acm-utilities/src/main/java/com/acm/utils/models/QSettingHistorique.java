/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QSettingHistorique is a Querydsl query type for SettingHistorique.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingHistorique extends EntityPathBase<SettingHistorique> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -633009290L;

	/** The Constant settingHistorique. */
	public static final QSettingHistorique settingHistorique =
			new QSettingHistorique("settingHistorique");

	/** The action. */
	public final StringPath action = createString("action");

	/** The date update. */
	public final DateTimePath<java.util.Date> dateUpdate =
			createDateTime("dateUpdate", java.util.Date.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id object. */
	public final NumberPath<Long> idObject = createNumber("idObject", Long.class);

	/** The new data. */
	public final StringPath newData = createString("newData");

	/** The table name. */
	public final StringPath tableName = createString("tableName");

	/** The updated by. */
	public final StringPath updatedBy = createString("updatedBy");

	/** The updated data. */
	public final StringPath updatedData = createString("updatedData");

	/**
	 * Instantiates a new q setting historique.
	 *
	 * @param variable the variable
	 */
	public QSettingHistorique(String variable) {

		super(SettingHistorique.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting historique.
	 *
	 * @param path the path
	 */
	public QSettingHistorique(Path<? extends SettingHistorique> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting historique.
	 *
	 * @param metadata the metadata
	 */
	public QSettingHistorique(PathMetadata metadata) {

		super(SettingHistorique.class, metadata);
	}

}
