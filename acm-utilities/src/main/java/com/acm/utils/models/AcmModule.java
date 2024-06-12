/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link AcmModule} class.
 *
 * @author kouali
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_MODULE")
public class AcmModule implements Serializable {

	/** The id module. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_MODULE", unique = true, nullable = false)
	private Long idModule;

	/** The module. */
	@Column(name = "MODULE")
	private String module;

	/** The collection instances. */
	@OneToMany(mappedBy = "racineId")
	private List<HabilitationIHMRoute> habilitationIHMRoute = new ArrayList<>();

	/**
	 * Gets the id module.
	 *
	 * @return the idModule
	 */
	public Long getIdModule() {

		return idModule;
	}

	/**
	 * Sets the id module.
	 *
	 * @param idModule the idModule to set
	 */
	public void setIdModule(Long idModule) {

		this.idModule = idModule;
	}

	/**
	 * Gets the module.
	 *
	 * @return the module
	 */
	public String getModule() {

		return module;
	}

	/**
	 * Gets the habilitation IHM route.
	 *
	 * @return the habilitationIHMRoute
	 */
	public List<HabilitationIHMRoute> getHabilitationIHMRoute() {

		return habilitationIHMRoute;
	}

	/**
	 * Sets the habilitation IHM route.
	 *
	 * @param habilitationIHMRoute the habilitationIHMRoute to set
	 */
	public void setHabilitationIHMRoute(List<HabilitationIHMRoute> habilitationIHMRoute) {

		this.habilitationIHMRoute = habilitationIHMRoute;
	}

	/**
	 * Sets the module.
	 *
	 * @param module the module to set
	 */
	public void setModule(String module) {

		this.module = module;
	}

}
