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
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QUserDefinedFieldGroup is a Querydsl query type for UserDefinedFieldGroup.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QUserDefinedFieldGroup extends EntityPathBase<UserDefinedFieldGroup> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -30761780L;

	/** The Constant userDefinedFieldGroup. */
	public static final QUserDefinedFieldGroup userDefinedFieldGroup =
			new QUserDefinedFieldGroup("userDefinedFieldGroup");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code. */
	public final StringPath code = createString("code");

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The customer type. */
	public final NumberPath<Integer> customerType = createNumber("customerType", Integer.class);

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

	/** The id UD group abacus. */
	public final NumberPath<Long> idUDGroupAbacus = createNumber("idUDGroupAbacus", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan id. */
	public final NumberPath<Long> loanId = createNumber("loanId", Long.class);

	/** The mondatory. */
	public final BooleanPath mondatory = createBoolean("mondatory");

	/** The product id. */
	public final StringPath productId = createString("productId");

	/** The category. */
	public final StringPath category = createString("category");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user defined fields. */
	public final SetPath<UserDefinedFields, QUserDefinedFields> userDefinedFields =
			this.<UserDefinedFields, QUserDefinedFields>createSet("userDefinedFields",
					UserDefinedFields.class, QUserDefinedFields.class, PathInits.DIRECT2);

	/**
	 * Instantiates a new q user defined field group.
	 *
	 * @param variable the variable
	 */
	public QUserDefinedFieldGroup(String variable) {

		super(UserDefinedFieldGroup.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q user defined field group.
	 *
	 * @param path the path
	 */
	public QUserDefinedFieldGroup(Path<? extends UserDefinedFieldGroup> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q user defined field group.
	 *
	 * @param metadata the metadata
	 */
	public QUserDefinedFieldGroup(PathMetadata metadata) {

		super(UserDefinedFieldGroup.class, metadata);
	}

}
