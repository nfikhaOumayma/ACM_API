/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link Groupe} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_GROUPE")
public class Groupe extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5906500483712021289L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_GROUPE", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The libelle. */
	@Column(name = "LIBELLE", nullable = false)
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The user profile ID extern. */
	@Column(name = "USER_PROFILE_ID_EXTERN")
	private Long userProfileIDExtern;

	/** The users. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "groupes")
	private Set<User> users = new HashSet<>();

	/** The groups users collection. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "participants")
	private Set<CollectionStep> collections = new HashSet<>();

	/** The loans participants. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "participants")
	private Set<WorkFlowStep> loansParticipants = new HashSet<>();

	/** The loans participants approval. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "approvers")
	private Set<WorkFlowStep> loansParticipantsApproval = new HashSet<>();

	/** The group loans instance. */
	@OneToMany(mappedBy = "groupe", cascade = CascadeType.ALL)
	private List<AcmLoanInstanceAcmGroupeApproval> groupLoansInstance = new ArrayList<>();

	/**
	 * Instantiates a new groupe.
	 */
	public Groupe() {

		/*
		 * Empty
		 */
	}

	/**
	 * Adds the user.
	 * 
	 * @author HaythemBenizid
	 * @param user the user
	 */
	public void addUser(User user) {

		users.add(user);
		user.getGroupes().add(this);
	}

	/**
	 * Removes the user.
	 * 
	 * @author HaythemBenizid
	 * @param user the user
	 */
	public void removeUser(User user) {

		users.remove(user);
		user.getGroupes().remove(this);
	}

	/**
	 * Instantiates a new groupe.
	 *
	 * @param code the code
	 */
	public Groupe(String code) {

		this.code = code;
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
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
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
	 * @param libelle the libelle to set
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
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the users.
	 *
	 * @return the users
	 */
	public Set<User> getUsers() {

		return users;
	}

	/**
	 * Sets the users.
	 *
	 * @param users the users to set
	 */
	public void setUsers(Set<User> users) {

		this.users = users;
	}

	/**
	 * Gets the user profile ID extern.
	 *
	 * @return the userProfileIDExtern
	 */
	public Long getUserProfileIDExtern() {

		return userProfileIDExtern;
	}

	/**
	 * Sets the user profile ID extern.
	 *
	 * @param userProfileIDExtern the userProfileIDExtern to set
	 */
	public void setUserProfileIDExtern(Long userProfileIDExtern) {

		this.userProfileIDExtern = userProfileIDExtern;
	}

	/**
	 * Gets the collections.
	 *
	 * @return the collections
	 */
	public Set<CollectionStep> getCollections() {

		return collections;
	}

	/**
	 * Sets the collections.
	 *
	 * @param collections the new collections
	 */
	public void setCollections(Set<CollectionStep> collections) {

		this.collections = collections;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.utils.models.GenericModel#toString()
	 */
	@Override
	public String toString() {

		return "Groupe [" + (id != null ? "id=" + id + ", " : "")
				+ (code != null ? "code=" + code + ", " : "")
				+ (libelle != null ? "libelle=" + libelle + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (userProfileIDExtern != null ? "userProfileIDExtern=" + userProfileIDExtern + ", "
						: "")
				+ (users != null ? "users=" + users : "") + "]";
	}

	/**
	 * Gets the loans participants.
	 *
	 * @return the loans participants
	 */
	public Set<WorkFlowStep> getLoansParticipants() {

		return loansParticipants;
	}

	/**
	 * Sets the loans participants.
	 *
	 * @param loansParticipants the new loans participants
	 */
	public void setLoansParticipants(Set<WorkFlowStep> loansParticipants) {

		this.loansParticipants = loansParticipants;
	}

	/**
	 * Gets the loans participants approval.
	 *
	 * @return the loans participants approval
	 */
	public Set<WorkFlowStep> getLoansParticipantsApproval() {

		return loansParticipantsApproval;
	}

	/**
	 * Sets the loans participants approval.
	 *
	 * @param loansParticipantsApproval the new loans participants approval
	 */
	public void setLoansParticipantsApproval(Set<WorkFlowStep> loansParticipantsApproval) {

		this.loansParticipantsApproval = loansParticipantsApproval;
	}

	/**
	 * Gets the group loans instance.
	 *
	 * @return the group loans instance
	 */
	public List<AcmLoanInstanceAcmGroupeApproval> getGroupLoansInstance() {

		return groupLoansInstance;
	}

	/**
	 * Sets the group loans instance.
	 *
	 * @param groupLoansInstance the new group loans instance
	 */
	public void setGroupLoansInstance(List<AcmLoanInstanceAcmGroupeApproval> groupLoansInstance) {

		this.groupLoansInstance = groupLoansInstance;
	}

}
