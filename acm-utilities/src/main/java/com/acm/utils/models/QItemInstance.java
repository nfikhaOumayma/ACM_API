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
 * QItemInstance is a Querydsl query type for ItemInstance.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QItemInstance extends EntityPathBase<ItemInstance> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 742905311L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant itemInstance. */
	public static final QItemInstance itemInstance = new QItemInstance("itemInstance");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The action user. */
	public final StringPath actionUser = createString("actionUser");

	/** The code statut item. */
	public final NumberPath<Long> codeStatutItem = createNumber("codeStatutItem", Long.class);

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

	/** The id work flow step. */
	public final NumberPath<Long> idWorkFlowStep = createNumber("idWorkFlowStep", Long.class);

	/** The ihm root. */
	public final StringPath ihmRoot = createString("ihmRoot");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The item. */
	public final QItem item;

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The order etape process. */
	public final NumberPath<Long> orderEtapeProcess = createNumber("orderEtapeProcess", Long.class);

	/** The statut item. */
	public final StringPath statutItem = createString("statutItem");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q item instance.
	 *
	 * @param variable the variable
	 */
	public QItemInstance(String variable) {

		this(ItemInstance.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q item instance.
	 *
	 * @param path the path
	 */
	public QItemInstance(Path<? extends ItemInstance> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q item instance.
	 *
	 * @param metadata the metadata
	 */
	public QItemInstance(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q item instance.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QItemInstance(PathMetadata metadata, PathInits inits) {

		this(ItemInstance.class, metadata, inits);
	}

	/**
	 * Instantiates a new q item instance.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QItemInstance(Class<? extends ItemInstance> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.item = inits.isInitialized("item") ? new QItem(forProperty("item"), inits.get("item"))
				: null;
	}

}
