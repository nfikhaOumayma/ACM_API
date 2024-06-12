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
 * QTransversHistorique is a Querydsl query type for TransversHistorique.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QTransversHistorique extends EntityPathBase<TransversHistorique> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1427837118L;

	/** The Constant transversHistorique. */
	public static final QTransversHistorique transversHistorique =
			new QTransversHistorique("transversHistorique");

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

	/** The methode. */
	public final StringPath methode = createString("methode");

	/** The object value. */
	public final StringPath objectValue = createString("objectValue");

	/** The request value. */
	public final StringPath requestValue = createString("requestValue");

	/** The response value. */
	public final StringPath responseValue = createString("responseValue");

	/** The status. */
	public final StringPath status = createString("status");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The uri. */
	public final StringPath uri = createString("uri");

	/**
	 * Instantiates a new q transvers historique.
	 *
	 * @param variable the variable
	 */
	public QTransversHistorique(String variable) {

		super(TransversHistorique.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q transvers historique.
	 *
	 * @param path the path
	 */
	public QTransversHistorique(Path<? extends TransversHistorique> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q transvers historique.
	 *
	 * @param metadata the metadata
	 */
	public QTransversHistorique(PathMetadata metadata) {

		super(TransversHistorique.class, metadata);
	}

}
