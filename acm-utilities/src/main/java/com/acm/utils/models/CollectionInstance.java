/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link CollectionInstance} class.
 *
 * @author idridi
 * @since 1.1.9
 */
@Entity
@Table(name = "ACM_COLLECTION_INSTANCE")
public class CollectionInstance extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6242479947776416157L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_COLLECTION_INSTANCE", unique = true, nullable = false)
	private Long id;

	/** The collection. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_COLLECTION")
	private AcmCollection collection;

	/** The id acm collection step. */
	@Column(name = "ID_ACM_COLLECTION_STEP", nullable = false)
	private Integer idAcmCollectionStep;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The statut collection. */
	@Column(name = "STATUT_COLLECTION")
	private String statutCollection;

	/** The ihm root. */
	@Column(name = "IHM_WEB_ROOT")
	private String ihmRoot;

	/** The client. */
	@Column(name = "CLIENT")
	private String client;

	/** The orderEtapeProcess. */
	@Column(name = "ORDER_ETAPE_PROCESS")
	private Integer orderEtapeProcess;

	/** The start date. */
	@Column(name = "START_DATE")
	private Integer startDate;

	/** After. */
	@Column(name = "AFTER_DATE")
	private String afterDate;

	/** The step name. */
	@Column(name = "STEP_NAME")
	private String stepName;

	/** The action user. */
	@Column(name = "ACTION_USER")
	private String actionUser;

	/** The Charge fees. */
	@OneToMany(mappedBy = "collectionInstance")
	private Set<ChargeFees> ChargeFees = new HashSet<>();

	/** The acm third party collection instances. */
	@ManyToMany(fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_COLLECTION_INSTANCE_ACM_THIRD_PARTY",
			joinColumns = {@JoinColumn(name = "ACM_COLLECTION_INSTANCE_ID")},
			inverseJoinColumns = {@JoinColumn(name = "ACM_THIRD_PARTY_ID")})
	private Set<AcmThirdParty> thirdParties = new HashSet<>();

	/**
	 * Gets the step name.
	 *
	 * @return the step name
	 */
	public String getStepName() {

		return stepName;
	}

	/**
	 * Sets the step name.
	 *
	 * @param stepName the new step name
	 */
	public void setStepName(String stepName) {

		this.stepName = stepName;
	}

	/**
	 * Gets the third parties.
	 *
	 * @return the third parties
	 */
	public Set<AcmThirdParty> getThirdParties() {

		return thirdParties;
	}

	/**
	 * Sets the third parties.
	 *
	 * @param thirdParties the new third parties
	 */
	public void setThirdParties(Set<AcmThirdParty> thirdParties) {

		this.thirdParties = thirdParties;
	}

	/**
	 * Instantiates a new collection instance.
	 */
	public CollectionInstance() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the collection.
	 *
	 * @return the collection
	 */
	public AcmCollection getCollection() {

		return collection;
	}

	/**
	 * Sets the collection.
	 *
	 * @param collection the new collection
	 */
	public void setCollection(AcmCollection collection) {

		this.collection = collection;
	}

	/**
	 * Gets the id acm collection step.
	 *
	 * @return the id acm collection step
	 */
	public Integer getIdAcmCollectionStep() {

		return idAcmCollectionStep;
	}

	/**
	 * Sets the id acm collection step.
	 *
	 * @param idAcmCollectionStep the new id acm collection step
	 */
	public void setIdAcmCollectionStep(Integer idAcmCollectionStep) {

		this.idAcmCollectionStep = idAcmCollectionStep;
	}

	/**
	 * Gets the libelle.
	 *
	 * @return the libelle
	 */
	public String getLibelle() {

		return libelle;
	}

	/**
	 * Sets the libelle.
	 *
	 * @param libelle the new libelle
	 */
	public void setLibelle(String libelle) {

		this.libelle = libelle;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the statut collection.
	 *
	 * @return the statut collection
	 */
	public String getStatutCollection() {

		return statutCollection;
	}

	/**
	 * Sets the statut collection.
	 *
	 * @param statutCollection the new statut collection
	 */
	public void setStatutCollection(String statutCollection) {

		this.statutCollection = statutCollection;
	}

	/**
	 * Gets the ihm root.
	 *
	 * @return the ihm root
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihm root.
	 *
	 * @param ihmRoot the new ihm root
	 */
	public void setIhmRoot(String ihmRoot) {

		this.ihmRoot = ihmRoot;
	}

	/**
	 * Gets the client.
	 *
	 * @return the client
	 */
	public String getClient() {

		return client;
	}

	/**
	 * Sets the client.
	 *
	 * @param client the new client
	 */
	public void setClient(String client) {

		this.client = client;
	}

	/**
	 * Gets the order etape process.
	 *
	 * @return the order etape process
	 */
	public Integer getOrderEtapeProcess() {

		return orderEtapeProcess;
	}

	/**
	 * Sets the order etape process.
	 *
	 * @param orderEtapeProcess the new order etape process
	 */
	public void setOrderEtapeProcess(Integer orderEtapeProcess) {

		this.orderEtapeProcess = orderEtapeProcess;
	}

	/**
	 * Gets the start date.
	 *
	 * @return the start date
	 */
	public Integer getStartDate() {

		return startDate;
	}

	/**
	 * Sets the start date.
	 *
	 * @param startDate the new start date
	 */
	public void setStartDate(Integer startDate) {

		this.startDate = startDate;
	}

	/**
	 * Gets the after date.
	 *
	 * @return the after date
	 */
	public String getAfterDate() {

		return afterDate;
	}

	/**
	 * Sets the after date.
	 *
	 * @param afterDate the new after date
	 */
	public void setAfterDate(String afterDate) {

		this.afterDate = afterDate;
	}

	/**
	 * Gets the action user.
	 *
	 * @return the action user
	 */
	public String getActionUser() {

		return actionUser;
	}

	/**
	 * Sets the action user.
	 *
	 * @param actionUser the new action user
	 */
	public void setActionUser(String actionUser) {

		this.actionUser = actionUser;
	}

	/**
	 * Gets the charge fees.
	 *
	 * @return the charge fees
	 */
	public Set<ChargeFees> getChargeFees() {

		return ChargeFees;
	}

	/**
	 * Sets the charge fees.
	 *
	 * @param chargeFees the new charge fees
	 */
	public void setChargeFees(Set<ChargeFees> chargeFees) {

		ChargeFees = chargeFees;
	}

}
