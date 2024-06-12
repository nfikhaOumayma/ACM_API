/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.ArrayPath;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QAcmDocumentsGed is a Querydsl query type for AcmDocumentsGed.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmDocumentsGed extends EntityPathBase<AcmDocumentsGed> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 643542306L;

	/** The Constant acmDocumentsGed. */
	public static final QAcmDocumentsGed acmDocumentsGed = new QAcmDocumentsGed("acmDocumentsGed");

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

	/** The document ged. */
	public final ArrayPath<byte[], Byte> documentGed = createArray("documentGed", byte[].class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id customer. */
	public final NumberPath<Long> idCustomer = createNumber("idCustomer", Long.class);

	/** The id document. */
	public final NumberPath<Long> idDocument = createNumber("idDocument", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan id. */
	public final NumberPath<Long> loanId = createNumber("loanId", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm documents ged.
	 *
	 * @param variable the variable
	 */
	public QAcmDocumentsGed(String variable) {

		super(AcmDocumentsGed.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm documents ged.
	 *
	 * @param path the path
	 */
	public QAcmDocumentsGed(Path<? extends AcmDocumentsGed> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm documents ged.
	 *
	 * @param metadata the metadata
	 */
	public QAcmDocumentsGed(PathMetadata metadata) {

		super(AcmDocumentsGed.class, metadata);
	}

}
