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
 * QAMLData is a Querydsl query type for AMLData.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAMLData extends EntityPathBase<AMLData> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 221889043L;

	/** The Constant aMLData. */
	public static final QAMLData aMLData = new QAMLData("aMLData");

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

	/** The date of birth. */
	public final StringPath dateOfBirth = createString("dateOfBirth");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The identity number. */
	public final StringPath identityNumber = createString("identityNumber");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The name. */
	public final StringPath name = createString("name");

	/** The reference case. */
	public final StringPath referenceCase = createString("referenceCase");

	/** The reference in file. */
	public final NumberPath<Long> referenceInFile = createNumber("referenceInFile", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The updated data. */
	public final StringPath updatedData = createString("updatedData");

	/**
	 * Instantiates a new QAML data.
	 *
	 * @param variable the variable
	 */
	public QAMLData(String variable) {

		super(AMLData.class, forVariable(variable));
	}

	/**
	 * Instantiates a new QAML data.
	 *
	 * @param path the path
	 */
	public QAMLData(Path<? extends AMLData> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new QAML data.
	 *
	 * @param metadata the metadata
	 */
	public QAMLData(PathMetadata metadata) {

		super(AMLData.class, metadata);
	}

}
