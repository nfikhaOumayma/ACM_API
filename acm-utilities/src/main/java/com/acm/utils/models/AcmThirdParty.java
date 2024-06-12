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
import javax.persistence.ManyToMany;
import javax.persistence.Table;

import com.acm.utils.enums.CollectionLegalThirdPartyType;

/**
 * The Class AcmThirdParty.
 */
@Entity
@Table(name = "ACM_THIRD_PARTY")
public class AcmThirdParty extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2613414464775720494L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_THIRD_PARTY", unique = true, nullable = false)
	private Long id;

	/** The first name. */
	@Column(name = "FIRST_NAME")
	private String firstName;

	/** The last name. */
	@Column(name = "LAST_NAME")
	private String lastName;

	/** The address party. */
	@Column(name = "ADDRESS_PARTY")
	private String addressParty;

	/** The email. */
	@Column(name = "EMAIL")
	private String email;

	/** The phone number. */
	@Column(name = "PHONE_NUMBER")
	private String phoneNumber;

	/** The access branches. */
	@Column(name = "ACCESS_BRANCHES")
	private String accessBranches;

	/** The Type Party : {@link CollectionLegalThirdPartyType} ENUMS. */
	@Column(name = "TYPE_PARTY")
	private String typeParty;

	/** The branchID. */
	@Column(name = "BRANCHID")
	private Integer branchID;

	/** The branch name. */
	@Column(name = "BRANCHE_NAME", length = 50)
	private String branchName;

	/** The branch description. */
	@Column(name = "BRANCHE_DESCRIPTION", length = 200)
	private String branchDescription;

	/** The type. */
	@Column(name = "TYPE", length = 512)
	private String type;

	/** The statut. */
	@Column(name = "STATUT", length = 512)
	private String statut;

	/** The pays. */
	@Column(name = "PAYS", length = 512)
	private String pays;

	/** The ville. */
	@Column(name = "VILLE", length = 512)
	private String ville;

	/** The code postal. */
	@Column(name = "CODE_POSTAL")
	private Integer code_postal;

	/** The numero rne. */
	@Column(name = "NUMERO_RNE")
	private Long numero_rne;

	/** The collections instances. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "thirdParties")
	private Set<CollectionInstance> collectionsInstances = new HashSet<>();

	/**
	 * Instantiates a new acm legal collection third party.
	 */
	public AcmThirdParty() {

	}

	/**
	 * Gets the branch ID.
	 *
	 * @return the branch ID
	 */
	public Integer getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 *
	 * @param branchID the new branch ID
	 */
	public void setBranchID(Integer branchID) {

		this.branchID = branchID;
	}

	/**
	 * Gets the branch name.
	 *
	 * @return the branch name
	 */
	public String getBranchName() {

		return branchName;
	}

	/**
	 * Sets the branch name.
	 *
	 * @param branchName the new branch name
	 */
	public void setBranchName(String branchName) {

		this.branchName = branchName;
	}

	/**
	 * Gets the branch description.
	 *
	 * @return the branch description
	 */
	public String getBranchDescription() {

		return branchDescription;
	}

	/**
	 * Sets the branch description.
	 *
	 * @param branchDescription the new branch description
	 */
	public void setBranchDescription(String branchDescription) {

		this.branchDescription = branchDescription;
	}

	/**
	 * Gets the collections instances.
	 *
	 * @return the collections instances
	 */
	public Set<CollectionInstance> getCollectionsInstances() {

		return collectionsInstances;
	}

	/**
	 * Sets the collections instances.
	 *
	 * @param collectionsInstances the new collections instances
	 */
	public void setCollectionsInstances(Set<CollectionInstance> collectionsInstances) {

		this.collectionsInstances = collectionsInstances;
	}

	/**
	 * Instantiates a new acm collection third party.
	 *
	 * @param id the id
	 * @param firstName the first name
	 * @param lastName the last name
	 * @param addressParty the address party
	 * @param email the email
	 * @param phoneNumber the phone number
	 * @param accessBranches the access branches
	 * @param typeParty the type party
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param collectionsInstances the collections instances
	 */
	public AcmThirdParty(Long id, String firstName, String lastName, String addressParty,
			String email, String phoneNumber, String accessBranches, String typeParty,
			Integer branchID, String branchName, String branchDescription,
			Set<CollectionInstance> collectionsInstances) {

		super();
		this.id = id;
		this.firstName = firstName;
		this.lastName = lastName;
		this.addressParty = addressParty;
		this.email = email;
		this.phoneNumber = phoneNumber;
		this.accessBranches = accessBranches;
		this.typeParty = typeParty;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
		this.collectionsInstances = collectionsInstances;
	}

	/**
	 * Instantiates a new acm third party.
	 *
	 * @param id the id
	 * @param firstName the first name
	 * @param lastName the last name
	 * @param addressParty the address party
	 * @param email the email
	 * @param phoneNumber the phone number
	 * @param accessBranches the access branches
	 * @param typeParty the type party
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param type the type
	 * @param statut the statut
	 * @param pays the pays
	 * @param ville the ville
	 * @param code_postal the code postal
	 * @param numero_rne the numero rne
	 * @param collectionsInstances the collections instances
	 */
	public AcmThirdParty(Long id, String firstName, String lastName, String addressParty,
			String email, String phoneNumber, String accessBranches, String typeParty,
			Integer branchID, String branchName, String branchDescription, String type,
			String statut, String pays, String ville, Integer code_postal, Long numero_rne,
			Set<CollectionInstance> collectionsInstances) {

		super();
		this.id = id;
		this.firstName = firstName;
		this.lastName = lastName;
		this.addressParty = addressParty;
		this.email = email;
		this.phoneNumber = phoneNumber;
		this.accessBranches = accessBranches;
		this.typeParty = typeParty;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
		this.type = type;
		this.statut = statut;
		this.pays = pays;
		this.ville = ville;
		this.code_postal = code_postal;
		this.numero_rne = numero_rne;
		this.collectionsInstances = collectionsInstances;
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
	 * Gets the first name.
	 *
	 * @return the first name
	 */
	public String getFirstName() {

		return firstName;
	}

	/**
	 * Sets the first name.
	 *
	 * @param firstName the new first name
	 */
	public void setFirstName(String firstName) {

		this.firstName = firstName;
	}

	/**
	 * Gets the last name.
	 *
	 * @return the last name
	 */
	public String getLastName() {

		return lastName;
	}

	/**
	 * Sets the last name.
	 *
	 * @param lastName the new last name
	 */
	public void setLastName(String lastName) {

		this.lastName = lastName;
	}

	/**
	 * Gets the address party.
	 *
	 * @return the address party
	 */
	public String getAddressParty() {

		return addressParty;
	}

	/**
	 * Sets the address party.
	 *
	 * @param addressParty the new address party
	 */
	public void setAddressParty(String addressParty) {

		this.addressParty = addressParty;
	}

	/**
	 * Gets the email.
	 *
	 * @return the email
	 */
	public String getEmail() {

		return email;
	}

	/**
	 * Sets the email.
	 *
	 * @param email the new email
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the phone number.
	 *
	 * @return the phone number
	 */
	public String getPhoneNumber() {

		return phoneNumber;
	}

	/**
	 * Sets the phone number.
	 *
	 * @param phoneNumber the new phone number
	 */
	public void setPhoneNumber(String phoneNumber) {

		this.phoneNumber = phoneNumber;
	}

	/**
	 * Gets the access branches.
	 *
	 * @return the access branches
	 */
	public String getAccessBranches() {

		return accessBranches;
	}

	/**
	 * Sets the access branches.
	 *
	 * @param accessBranches the new access branches
	 */
	public void setAccessBranches(String accessBranches) {

		this.accessBranches = accessBranches;
	}

	/**
	 * Gets the type party.
	 *
	 * @return the type party
	 */
	public String getTypeParty() {

		return typeParty;
	}

	/**
	 * Sets the type party.
	 *
	 * @param typeParty the new type party
	 */
	public void setTypeParty(String typeParty) {

		this.typeParty = typeParty;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public String getType() {

		return type;
	}

	/**
	 * Sets the type.
	 *
	 * @param type the new type
	 */
	public void setType(String type) {

		this.type = type;
	}

	/**
	 * Gets the statut.
	 *
	 * @return the statut
	 */
	public String getStatut() {

		return statut;
	}

	/**
	 * Sets the statut.
	 *
	 * @param statut the new statut
	 */
	public void setStatut(String statut) {

		this.statut = statut;
	}

	/**
	 * Gets the pays.
	 *
	 * @return the pays
	 */
	public String getPays() {

		return pays;
	}

	/**
	 * Sets the pays.
	 *
	 * @param pays the new pays
	 */
	public void setPays(String pays) {

		this.pays = pays;
	}

	/**
	 * Gets the ville.
	 *
	 * @return the ville
	 */
	public String getVille() {

		return ville;
	}

	/**
	 * Sets the ville.
	 *
	 * @param ville the new ville
	 */
	public void setVille(String ville) {

		this.ville = ville;
	}

	/**
	 * Gets the code postal.
	 *
	 * @return the code postal
	 */
	public Integer getCode_postal() {

		return code_postal;
	}

	/**
	 * Sets the code postal.
	 *
	 * @param code_postal the new code postal
	 */
	public void setCode_postal(Integer code_postal) {

		this.code_postal = code_postal;
	}

	/**
	 * Gets the numero rne.
	 *
	 * @return the numero rne
	 */
	public Long getNumero_rne() {

		return numero_rne;
	}

	/**
	 * Sets the numero rne.
	 *
	 * @param numero_rne the new numero rne
	 */
	public void setNumero_rne(Long numero_rne) {

		this.numero_rne = numero_rne;
	}

}
