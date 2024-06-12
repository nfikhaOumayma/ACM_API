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
 * QCollectionNote is a Querydsl query type for CollectionNote.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCollectionNote extends EntityPathBase<CollectionNote> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2043514631L;

	/** The Constant collectionNote. */
	public static final QCollectionNote collectionNote = new QCollectionNote("collectionNote");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The action. */
	public final StringPath action = createString("action");

	/** The collection id. */
	public final NumberPath<Long> collectionId = createNumber("collectionId", Long.class);

	/** The comment. */
	public final StringPath comment = createString("comment");

	/** The date insertion. */
	public final DateTimePath<java.util.Date> dateInsertion =
			createDateTime("dateInsertion", java.util.Date.class);

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	public final StringPath insertBy = createString("insertBy");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q collection note.
	 *
	 * @param variable the variable
	 */
	public QCollectionNote(String variable) {

		super(CollectionNote.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q collection note.
	 *
	 * @param path the path
	 */
	public QCollectionNote(Path<? extends CollectionNote> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q collection note.
	 *
	 * @param metadata the metadata
	 */
	public QCollectionNote(PathMetadata metadata) {

		super(CollectionNote.class, metadata);
	}

}
