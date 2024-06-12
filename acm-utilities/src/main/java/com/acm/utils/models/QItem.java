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
 * QItem is a Querydsl query type for Item.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QItem extends EntityPathBase<Item> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1924376362L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant item. */
	public static final QItem item = new QItem("item");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The actual step. */
	public final NumberPath<Long> actualStep = createNumber("actualStep", Long.class);

	/** The actual step instance. */
	public final NumberPath<Long> actualStepInstance =
			createNumber("actualStepInstance", Long.class);

	/** The branches. */
	public final StringPath branches = createString("branches");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The generic work flow object. */
	public final QGenericWorkFlowObject genericWorkFlowObject;

	/** The group owner. */
	public final StringPath groupOwner = createString("groupOwner");

	/** The group owner name. */
	public final StringPath groupOwnerName = createString("groupOwnerName");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The item instances. */
	public final SetPath<ItemInstance, QItemInstance> itemInstances =
			this.<ItemInstance, QItemInstance>createSet("itemInstances", ItemInstance.class,
					QItemInstance.class, PathInits.DIRECT2);

	/** The owner. */
	public final StringPath owner = createString("owner");

	/** The owner email. */
	public final StringPath ownerEmail = createString("ownerEmail");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The portfolio id. */
	public final NumberPath<Long> portfolioId = createNumber("portfolioId", Long.class);

	/** The review from step. */
	public final NumberPath<Long> reviewFromStep = createNumber("reviewFromStep", Long.class);

	/** The status. */
	public final NumberPath<Integer> status = createNumber("status", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q item.
	 *
	 * @param variable the variable
	 */
	public QItem(String variable) {

		this(Item.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q item.
	 *
	 * @param path the path
	 */
	public QItem(Path<? extends Item> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q item.
	 *
	 * @param metadata the metadata
	 */
	public QItem(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q item.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QItem(PathMetadata metadata, PathInits inits) {

		this(Item.class, metadata, inits);
	}

	/**
	 * Instantiates a new q item.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QItem(Class<? extends Item> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.genericWorkFlowObject = inits.isInitialized("genericWorkFlowObject")
				? new QGenericWorkFlowObject(forProperty("genericWorkFlowObject"))
				: null;
	}

}
