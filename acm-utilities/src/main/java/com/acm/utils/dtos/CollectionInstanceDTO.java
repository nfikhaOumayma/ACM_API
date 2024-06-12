/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

import org.dozer.Mapping;

/**
 * {@link CollectionInstanceDTO} class.
 *
 * @author idridi
 * @since 1.1.9
 */
public class CollectionInstanceDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8760574002400548714L;

	/** The id. */
	private Long id;

	/** The id collection. */
	@Mapping("collection.id")
	private Long idCollection;

	/** The id acm collection step. */
	private Integer idAcmCollectionStep;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The statut collection. */
	private String statutCollection;

	/** The ihm root. */
	private String ihmRoot;

	/** The client. */
	private String client;

	/** The orderEtapeProcess. */
	private Integer orderEtapeProcess;

	/** The enabled. */
	private Boolean enabled;

	/** The start date. */
	private Integer startDate;

	/** afterDate. */
	private String afterDate;

	/** The step name. */
	private String stepName;

	/** The action user. */
	private String actionUser;

	/** The acm third party collection instances. */
	@Mapping("thirdParties")
	private List<AcmThirdPartyDTO> thirdParties;

	/**
	 * Instantiates a new collection instance DTO.
	 */
	public CollectionInstanceDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the third parties.
	 *
	 * @return the third parties
	 */
	public List<AcmThirdPartyDTO> getThirdParties() {

		return thirdParties;
	}

	/**
	 * Sets the third parties.
	 *
	 * @param thirdParties the new third parties
	 */
	public void setThirdParties(List<AcmThirdPartyDTO> thirdParties) {

		this.thirdParties = thirdParties;
	}

	/**
	 * Instantiates a new collection instance DTO.
	 *
	 * @param idCollection the id collection
	 * @param idAcmCollectionStep the id acm collection step
	 * @param libelle the libelle
	 * @param description the description
	 * @param statutCollection the statut collection
	 * @param ihmRoot the ihm root
	 * @param client the client
	 * @param orderEtapeProcess the order etape process
	 * @param enabled the enabled
	 * @param startDate the start date
	 * @param afterDate the after date
	 * @param stepName the step name
	 */
	public CollectionInstanceDTO(Long idCollection, Integer idAcmCollectionStep, String libelle,
			String description, String statutCollection, String ihmRoot, String client,
			Integer orderEtapeProcess, Boolean enabled, Integer startDate, String afterDate,
			String stepName) {

		this.idCollection = idCollection;
		this.idAcmCollectionStep = idAcmCollectionStep;
		this.libelle = libelle;
		this.description = description;
		this.statutCollection = statutCollection;
		this.ihmRoot = ihmRoot;
		this.client = client;
		this.orderEtapeProcess = orderEtapeProcess;
		this.enabled = enabled;
		this.startDate = startDate;
		this.afterDate = afterDate;
		this.stepName = stepName;
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
	 * Gets the id collection.
	 *
	 * @return the id collection
	 */
	public Long getIdCollection() {

		return idCollection;
	}

	/**
	 * Sets the id collection.
	 *
	 * @param idCollection the new id collection
	 */
	public void setIdCollection(Long idCollection) {

		this.idCollection = idCollection;
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
	 * @param code the new id acm collection step
	 */
	public void setIdAcmCollectionStep(Integer code) {

		this.idAcmCollectionStep = code;
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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
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

}
