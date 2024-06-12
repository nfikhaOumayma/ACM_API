/*
 * 
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

// TODO: Auto-generated Javadoc
/**
 * The Class QClaimNote.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QClaimNote extends EntityPathBase<ClaimNote> {
	

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1221447622309473416L;
	
    
	/** The Constant claimNote. */
	public static final QClaimNote claimNote = new QClaimNote("claimNote");
    
	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);
	
	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;
	

	/** The visibility. */
	public final StringPath visibility = createString("visibility");

	/** The claim id. */
	public final NumberPath<Long> claimId = createNumber("claimId", Long.class);

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
	 * Instantiates a new q claim note.
	 *
	 * @param variable the variable
	 */
	public QClaimNote(String variable) {
		super(ClaimNote.class, forVariable(variable));
	}


	/**
	 * Instantiates a new q claim note.
	 *
	 * @param path the path
	 */
	public QClaimNote(Path<? extends ClaimNote> path) {

		super(path.getType(), path.getMetadata());
	}


	/**
	 * Instantiates a new q claim note.
	 *
	 * @param metadata the metadata
	 */
	public QClaimNote(PathMetadata metadata) {

		super(ClaimNote.class, metadata);
	}
}
