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
 * QCollectionStep is a Querydsl query type for CollectionStep.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCollectionStep extends EntityPathBase<CollectionStep> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2043667937L;

	/** The Constant collectionStep. */
	public static final QCollectionStep collectionStep = new QCollectionStep("collectionStep");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The after date. */
	public final StringPath afterDate = createString("afterDate");

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

	/** The code acm template sms. */
	public final StringPath codeAcmTemplateSms = createString("codeAcmTemplateSms");

	/** The code statut loan. */
	public final StringPath codeStatutLoan = createString("codeStatutLoan");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The documents. */
	public final SetPath<SettingDocumentProduct, QSettingDocumentProduct> documents =
			this.<SettingDocumentProduct, QSettingDocumentProduct>createSet("documents",
					SettingDocumentProduct.class, QSettingDocumentProduct.class, PathInits.DIRECT2);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The generation task. */
	public final BooleanPath generationTask = createBoolean("generationTask");

	/** The group code. */
	public final StringPath groupCode = createString("groupCode");

	/** The id collection step. */
	public final NumberPath<Long> idCollectionStep = createNumber("idCollectionStep", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The late date. */
	public final NumberPath<Integer> lateDate = createNumber("lateDate", Integer.class);

	/** The list charge fees. */
	public final SetPath<SettingChargeFee, QSettingChargeFee> listChargeFees =
			this.<SettingChargeFee, QSettingChargeFee>createSet("listChargeFees",
					SettingChargeFee.class, QSettingChargeFee.class, PathInits.DIRECT2);

	/** The order. */
	public final NumberPath<Long> order = createNumber("order", Long.class);

	/** The participants. */
	public final SetPath<Groupe, QGroupe> participants = this.<Groupe, QGroupe>createSet(
			"participants", Groupe.class, QGroupe.class, PathInits.DIRECT2);

	/** The previous step. */
	public final StringPath previousStep = createString("previousStep");

	/** The process. */
	public final StringPath process = createString("process");

	/** The process version. */
	public final NumberPath<Long> processVersion = createNumber("processVersion", Long.class);

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The reminder. */
	public final NumberPath<Integer> reminder = createNumber("reminder", Integer.class);

	/** The reminder sup. */
	public final NumberPath<Integer> reminderSup = createNumber("reminderSup", Integer.class);

	/** The screen. */
	public final StringPath screen = createString("screen");

	/** The start date. */
	public final NumberPath<Integer> startDate = createNumber("startDate", Integer.class);

	/** The step tab. */
	public final StringPath step_tab = createString("step_tab");

	/** The step name. */
	public final StringPath stepName = createString("stepName");

	/** The step type. */
	public final StringPath stepType = createString("stepType");

	/** The type third party. */
	public final StringPath typeThirdParty = createString("typeThirdParty");

	/** The unpaid amount. */
	public final NumberPath<java.math.BigDecimal> unpaidAmount =
			createNumber("unpaidAmount", java.math.BigDecimal.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user group. */
	public final StringPath userGroup = createString("userGroup");

	/**
	 * Instantiates a new q collection step.
	 *
	 * @param variable the variable
	 */
	public QCollectionStep(String variable) {

		super(CollectionStep.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q collection step.
	 *
	 * @param path the path
	 */
	public QCollectionStep(Path<? extends CollectionStep> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q collection step.
	 *
	 * @param metadata the metadata
	 */
	public QCollectionStep(PathMetadata metadata) {

		super(CollectionStep.class, metadata);
	}

}
