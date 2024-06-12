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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QAcmConditionnalApprove is a Querydsl query type for AcmConditionnalApprove.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmConditionnalApprove extends EntityPathBase<AcmConditionnalApprove> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 698344731L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmConditionnalApprove. */
	public static final QAcmConditionnalApprove acmConditionnalApprove =
			new QAcmConditionnalApprove("acmConditionnalApprove");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The approval condition date. */
	public final DateTimePath<java.util.Date> approvalConditionDate =
			createDateTime("approvalConditionDate", java.util.Date.class);

	/** The calendar event approve. */
	public final QCalendarEvent calendarEventApprove;

	/** The calendar event approve validator. */
	public final QCalendarEvent calendarEventApproveValidator;

	/** The conditionnal validation. */
	public final BooleanPath conditionnalValidation = createBoolean("conditionnalValidation");

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

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The item. */
	public final QItem item;

	/** The loan. */
	public final QLoan loan;

	/** The status. */
	public final StringPath status = createString("status");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user. */
	public final QUser user;

	/** The username inserted by. */
	public final StringPath usernameInsertedBy = createString("usernameInsertedBy");

	/**
	 * Instantiates a new q acm conditionnal approve.
	 *
	 * @param variable the variable
	 */
	public QAcmConditionnalApprove(String variable) {

		this(AcmConditionnalApprove.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm conditionnal approve.
	 *
	 * @param path the path
	 */
	public QAcmConditionnalApprove(Path<? extends AcmConditionnalApprove> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm conditionnal approve.
	 *
	 * @param metadata the metadata
	 */
	public QAcmConditionnalApprove(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm conditionnal approve.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmConditionnalApprove(PathMetadata metadata, PathInits inits) {

		this(AcmConditionnalApprove.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm conditionnal approve.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmConditionnalApprove(Class<? extends AcmConditionnalApprove> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.calendarEventApprove = inits.isInitialized("calendarEventApprove")
				? new QCalendarEvent(forProperty("calendarEventApprove"))
				: null;
		this.calendarEventApproveValidator = inits.isInitialized("calendarEventApproveValidator")
				? new QCalendarEvent(forProperty("calendarEventApproveValidator"))
				: null;
		this.item = inits.isInitialized("item") ? new QItem(forProperty("item"), inits.get("item"))
				: null;
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
		this.user = inits.isInitialized("user") ? new QUser(forProperty("user")) : null;
	}

}
