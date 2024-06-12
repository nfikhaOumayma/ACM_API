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
 * QGenericModel is a Querydsl query type for GenericModel.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Generated("com.querydsl.codegen.SupertypeSerializer")
public class QGenericModel extends EntityPathBase<GenericModel> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -720128855L;

	/** The Constant genericModel. */
	public static final QGenericModel genericModel = new QGenericModel("genericModel");

	/** The acm version. */
	public final NumberPath<Integer> acmVersion = createNumber("acmVersion", Integer.class);

	/** The date insertion. */
	public final DateTimePath<java.util.Date> dateInsertion =
			createDateTime("dateInsertion", java.util.Date.class);

	/** The date last update. */
	public final DateTimePath<java.util.Date> dateLastUpdate =
			createDateTime("dateLastUpdate", java.util.Date.class);

	/** The enabled. */
	public final BooleanPath enabled = createBoolean("enabled");

	/** The insert by. */
	public final StringPath insertBy = createString("insertBy");

	/** The updated by. */
	public final StringPath updatedBy = createString("updatedBy");

	/**
	 * Instantiates a new q generic model.
	 *
	 * @param variable the variable
	 */
	public QGenericModel(String variable) {

		super(GenericModel.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q generic model.
	 *
	 * @param path the path
	 */
	public QGenericModel(Path<? extends GenericModel> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q generic model.
	 *
	 * @param metadata the metadata
	 */
	public QGenericModel(PathMetadata metadata) {

		super(GenericModel.class, metadata);
	}

}
