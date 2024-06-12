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

// TODO: Auto-generated Javadoc
/**
 * QCalendarEvent is a Querydsl query type for CalendarEvent.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCalendarEvent extends EntityPathBase<CalendarEvent> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1163811781L;

	/** The Constant calendarEvent. */
	public static final QCalendarEvent calendarEvent = new QCalendarEvent("calendarEvent");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The category. */
	public final StringPath category = createString("category");

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The date debut. */
	public final DateTimePath<java.util.Date> dateDebut =
			createDateTime("dateDebut", java.util.Date.class);

	/** The date fin. */
	public final DateTimePath<java.util.Date> dateFin =
			createDateTime("dateFin", java.util.Date.class);

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

	/** The id collection. */
	public final NumberPath<Long> idCollection = createNumber("idCollection", Long.class);

	/** The id claim. */
	public final NumberPath<Long> idClaim = createNumber("idClaim", Long.class);

	/** The id customer extern. */
	public final NumberPath<Long> idCustomerExtern = createNumber("idCustomerExtern", Long.class);

	/** The id item. */
	public final NumberPath<Long> idItem = createNumber("idItem", Long.class);

	/** The id loan extern. */
	public final NumberPath<Long> idLoanExtern = createNumber("idLoanExtern", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libelle event. */
	public final StringPath libelleEvent = createString("libelleEvent");

	/** The notifications. */
	public final SetPath<Notifications, QNotifications> notifications =
			this.<Notifications, QNotifications>createSet("notifications", Notifications.class,
					QNotifications.class, PathInits.DIRECT2);

	/** The participant. */
	public final StringPath participant = createString("participant");

	/** The phone numer. */
	public final StringPath phoneNumer = createString("phoneNumer");

	/** The place. */
	public final StringPath place = createString("place");

	/** The statut. */
	public final StringPath statut = createString("statut");

	/** The step name. */
	public final StringPath stepName = createString("stepName");

	/** The type event. */
	public final StringPath typeEvent = createString("typeEvent");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user full name. */
	public final StringPath userFullName = createString("userFullName");

	/** The username. */
	public final StringPath username = createString("username");

	/**
	 * Instantiates a new q calendar event.
	 *
	 * @param variable the variable
	 */
	public QCalendarEvent(String variable) {

		super(CalendarEvent.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q calendar event.
	 *
	 * @param path the path
	 */
	public QCalendarEvent(Path<? extends CalendarEvent> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q calendar event.
	 *
	 * @param metadata the metadata
	 */
	public QCalendarEvent(PathMetadata metadata) {

		super(CalendarEvent.class, metadata);
	}

}
