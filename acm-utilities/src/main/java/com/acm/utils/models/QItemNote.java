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
 * QItemNote is a Querydsl query type for ItemNote.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QItemNote extends EntityPathBase<ItemNote> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 352134716L;

	/** The Constant itemNote. */
	public static final QItemNote itemNote = new QItemNote("itemNote");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The action. */
	public final StringPath action = createString("action");

	/** The comment. */
	public final StringPath comment = createString("comment");

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

	/** The item id. */
	public final NumberPath<Long> itemId = createNumber("itemId", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q item note.
	 *
	 * @param variable the variable
	 */
	public QItemNote(String variable) {

		super(ItemNote.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q item note.
	 *
	 * @param path the path
	 */
	public QItemNote(Path<? extends ItemNote> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q item note.
	 *
	 * @param metadata the metadata
	 */
	public QItemNote(PathMetadata metadata) {

		super(ItemNote.class, metadata);
	}

}
