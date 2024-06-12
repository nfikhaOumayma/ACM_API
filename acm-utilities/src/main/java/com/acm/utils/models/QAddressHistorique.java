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
 * QAddressHistorique is a Querydsl query type for AddressHistorique.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAddressHistorique extends EntityPathBase<AddressHistorique> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1383324698L;

	/** The Constant addressHistorique. */
	public static final QAddressHistorique addressHistorique =
			new QAddressHistorique("addressHistorique");

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

	/** The id address ACM. */
	public final NumberPath<Long> idAddressACM = createNumber("idAddressACM", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The new address. */
	public final StringPath newAddress = createString("newAddress");

	/** The old address. */
	public final StringPath oldAddress = createString("oldAddress");

	/** The reason update. */
	public final StringPath reasonUpdate = createString("reasonUpdate");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q address historique.
	 *
	 * @param variable the variable
	 */
	public QAddressHistorique(String variable) {

		super(AddressHistorique.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q address historique.
	 *
	 * @param path the path
	 */
	public QAddressHistorique(Path<? extends AddressHistorique> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q address historique.
	 *
	 * @param metadata the metadata
	 */
	public QAddressHistorique(PathMetadata metadata) {

		super(AddressHistorique.class, metadata);
	}

}
