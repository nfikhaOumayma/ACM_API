/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.dozer.Mapping;

import com.acm.utils.models.HabilitationIHMRoute;

/**
 * {@link HabilitationIHMRoute} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public class HabilitationIHMRouteDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2999939799892098275L;

	/** The id. */
	private Long id;

	/** The client. */
	private String client;

	/** The code IHM route. */
	private String codeIHMRoute;

	/** The ihm route. */
	private String ihmRoute;

	/** The description. */
	private String description;

	/** The settings workflow. */
	private boolean settingsWorkflow = Boolean.TRUE;

	/** The habilitation ihm button DT os. */
	@Mapping("habilitationIHMButtons")
	private List<HabilitationIHMButtonDTO> habilitationIHMButtonDTO = new ArrayList<>();

	/** The forms DTO. */
	@Mapping("forms")
	private List<AcmIhmFormDTO> formsDTO = new ArrayList<>();

	/**
	 * Gets the settings workflow.
	 *
	 * @return the settings workflow
	 */
	public boolean getSettingsWorkflow() {

		return settingsWorkflow;
	}

	/**
	 * Sets the settings workflow.
	 *
	 * @param settingsWorkflow the new settings workflow
	 */
	public void setSettingsWorkflow(boolean settingsWorkflow) {

		this.settingsWorkflow = settingsWorkflow;
	}

	/** The racine id. */
	private AcmModuleDTO racineId;

	/**
	 * Instantiates a new habilitation IHM route DTO.
	 */
	public HabilitationIHMRouteDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new habilitation IHM route DTO.
	 *
	 * @param codeIHMRoute the code IHM route
	 */
	public HabilitationIHMRouteDTO(String codeIHMRoute) {

		this.codeIHMRoute = codeIHMRoute;
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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
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
	 * @param client the client to set
	 */
	public void setClient(String client) {

		this.client = client;
	}

	/**
	 * Gets the code IHM route.
	 *
	 * @return the codeIHMRoute
	 */
	public String getCodeIHMRoute() {

		return codeIHMRoute;
	}

	/**
	 * Sets the code IHM route.
	 *
	 * @param codeIHMRoute the codeIHMRoute to set
	 */
	public void setCodeIHMRoute(String codeIHMRoute) {

		this.codeIHMRoute = codeIHMRoute;
	}

	/**
	 * Gets the ihm route.
	 *
	 * @return the ihmRoute
	 */
	public String getIhmRoute() {

		return ihmRoute;
	}

	/**
	 * Sets the ihm route.
	 *
	 * @param ihmRoute the ihmRoute to set
	 */
	public void setIhmRoute(String ihmRoute) {

		this.ihmRoute = ihmRoute;
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
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the racine id.
	 *
	 * @return the racineId
	 */
	public AcmModuleDTO getRacineId() {

		return racineId;
	}

	/**
	 * Sets the racine id.
	 *
	 * @param racineId the racineId to set
	 */
	public void setRacineId(AcmModuleDTO racineId) {

		this.racineId = racineId;
	}

	/**
	 * Gets the habilitation ihm button.
	 * 
	 * @return the habilitationIHMButtonDTO
	 */
	public List<HabilitationIHMButtonDTO> getHabilitationIHMButtonDTO() {

		return habilitationIHMButtonDTO;
	}

	/**
	 * Sets the habilitation ihm button.
	 * 
	 * @param habilitationIHMButtonDTO the habilitationIHMButtonDTO to set
	 */
	public void setHabilitationIHMButtonDTO(
			List<HabilitationIHMButtonDTO> habilitationIHMButtonDTO) {

		this.habilitationIHMButtonDTO = habilitationIHMButtonDTO;
	}

	/**
	 * Gets the forms DTO.
	 *
	 * @return the forms DTO
	 */
	public List<AcmIhmFormDTO> getFormsDTO() {

		return formsDTO;
	}

	/**
	 * Sets the forms DTO.
	 *
	 * @param formsDTO the new forms DTO
	 */
	public void setFormsDTO(List<AcmIhmFormDTO> formsDTO) {

		this.formsDTO = formsDTO;
	}

}
