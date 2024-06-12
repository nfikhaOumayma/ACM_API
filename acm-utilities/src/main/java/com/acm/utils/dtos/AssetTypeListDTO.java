package com.acm.utils.dtos;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AssetTypeListDTO.
 */
public class AssetTypeListDTO {

	/** The id. */
	private Long id;

	/** The libelle. */
	private String libelle;

	/** The id parent. */
	private Long id_parent;

	/**
	 * Instantiates a new asset type list DTO.
	 *
	 * @param id the id
	 * @param libelle the libelle
	 * @param id_parent the id parent
	 */
	@JsonCreator
	public AssetTypeListDTO(@JsonProperty("id") Long id, @JsonProperty("libelle") String libelle,
			@JsonProperty("id_parent") Long id_parent) {

		this.id = id;
		this.libelle = libelle;
		this.id_parent = id_parent;
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
	 * Gets the id parent.
	 *
	 * @return the id parent
	 */
	public Long getId_parent() {

		return id_parent;
	}

	/**
	 * Sets the id parent.
	 *
	 * @param id_parent the new id parent
	 */
	public void setId_parent(Long id_parent) {

		this.id_parent = id_parent;
	}

}
