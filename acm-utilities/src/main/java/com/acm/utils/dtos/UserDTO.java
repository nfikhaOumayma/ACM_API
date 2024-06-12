/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;
import java.util.Set;

import org.dozer.Mapping;

import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.enums.UserLangValues;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * the {@link UserDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class UserDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8165418683173941662L;

	/** The login. */
	@Mapping("username")
	private String login;

	/** The nom. */
	private String nom;

	/** The prenom. */
	private String prenom;

	/** The email. */
	private String email;

	/** The full name => name + surname + role . */
	private String fullName;

	/** The simple name => name + surname. */
	private String simpleName;

	/** The account portfolio id. */
	private Long accountPortfolioId;

	/** The responsable id. */
	private String responsableId;

	/** The user extern id. */
	private Long userExternId;

	/** The user profil id. */
	private Long userProfilId;

	/** The active. */
	private Boolean active;

	/** The new entry : variable used in Batch to identify the new entry. */
	private Boolean newEntry;

	/**
	 * The type user : {@link UserHierarchicalType} CONNECTED_USER / RESPONSIBLE / COLLABORATORS.
	 */
	private String typeUser;

	/** The branchID. */
	private Integer branchID;

	/** The branch name. */
	private String branchName;

	/** The branch description. */
	private String branchDescription;

	/** The pwd. */
	private String pwd;

	/** The pwd new. */
	private String pwdNew;

	/** The Pwd confirm. */
	private String pwdConfirm;

	/** The enabled. */
	private Boolean enabled;

	/** The customer id. */
	private Long customerId;

	/** The access branches. */
	private String accessBranches;

	/** The groupe code. */
	private String groupeCode;

	/** The temporary pwd. */
	private Boolean temporaryPwd;

	/** The category : {@link UserCategory} ENUMS. */
	private String category;

	/** The defaultLang : {@link UserLangValues} ENUMS. */
	private String defaultLang;

	/** The groupes. */
	private Set<GroupeDTO> groupes;

	/** Portfolio name. */
	private String portfolioName;

	/** The old responsible id. */
	private String oldResponsibleId;

	/** The hiring date. */
	private Date hiringDate;

	/** The resigning date. */
	private Date resigningDate;

	/** The old responsable name. */
	private String oldResponsableName;

	/** The temporary responsable. */
	private Boolean temporaryResponsable;

	/** The change all responsible. */
	private Boolean changeAllResponsible;

	/** The login and name. */
	private String loginAndName;

	/** The employee id. */
	private String employeeId;
	
	/** The password expire date. */
	private Date pwdExpiryDate;

	/**
	 * Instantiates a new user DTO.
	 */
	public UserDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new user DTO.
	 *
	 * @param login the login
	 */
	public UserDTO(String login) {

		this.login = login;
	}

	/**
	 * Instantiates a new user DTO.
	 *
	 * @param login the login
	 * @param nom the nom
	 * @param prenom the prenom
	 * @param email the email
	 */
	public UserDTO(String login, String nom, String prenom, String email) {

		this.login = login;
		this.nom = nom;
		this.prenom = prenom;
		this.email = email;
	}

	/**
	 * Instantiates a new user DTO : used ONLY in create IB user.
	 *
	 * @param login the login
	 * @param nom the nom
	 * @param prenom the prenom
	 * @param email the email
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param customerId the customer id
	 */
	public UserDTO(String login, String nom, String prenom, String email, Integer branchID,
			String branchName, String branchDescription, Long customerId) {

		this.login = login;
		this.nom = nom;
		this.prenom = prenom;
		this.email = email;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
		this.customerId = customerId;
	}

	/**
	 * Instantiates a new user DTO.
	 *
	 * @param branchID the branch ID
	 */
	public UserDTO(Integer branchID) {

		this.branchID = branchID;
	}

	/**
	 * Instantiates a new user DTO used in class : UserDTOAbacusRowMapper.
	 *
	 * @param login the login
	 * @param accountPortfolioId the account portfolio id
	 * @param responsableId the responsable id
	 * @param userExternId the user extern id
	 * @param userProfilId the user profil id
	 * @param active the active
	 * @param branchID the branchID
	 * @param branchName the branchName
	 * @param branchDescription the branchDescription
	 */
	public UserDTO(String login, Long accountPortfolioId, String responsableId, Long userExternId,
			Long userProfilId, Boolean active, Integer branchID, String branchName,
			String branchDescription) {

		this.login = login;
		this.accountPortfolioId = accountPortfolioId;
		this.responsableId = responsableId;
		this.userProfilId = userProfilId;
		this.active = active;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;

	}

	/**
	 * Instantiates a new user DTO (used in BATCH).
	 *
	 * @param login the login
	 * @param accountPortfolioId the account portfolio id
	 * @param userExternId the user extern id
	 * @param userProfilId the user profil id
	 * @param branchID the branchID
	 * @param branchName the branchName
	 * @param branchDescription the branchDescription
	 */
	public UserDTO(String login, Long accountPortfolioId, Long userExternId, Long userProfilId,
			Integer branchID, String branchName, String branchDescription) {

		this.login = login;
		this.accountPortfolioId = accountPortfolioId;
		this.userExternId = userExternId;
		this.userProfilId = userProfilId;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
	}

	/**
	 * Gets the login.
	 *
	 * @return the login
	 */
	public String getLogin() {

		return login;
	}

	/**
	 * Sets the login.
	 *
	 * @param login the new login
	 */
	public void setLogin(String login) {

		this.login = login;
	}

	/**
	 * Gets the nom.
	 *
	 * @return the nom
	 */
	public String getNom() {

		return nom;
	}

	/**
	 * Sets the nom.
	 *
	 * @param nom the nom to set
	 */
	public void setNom(String nom) {

		this.nom = nom;
	}

	/**
	 * Gets the prenom.
	 *
	 * @return the prenom
	 */
	public String getPrenom() {

		return prenom;
	}

	/**
	 * Sets the prenom.
	 *
	 * @param prenom the prenom to set
	 */
	public void setPrenom(String prenom) {

		this.prenom = prenom;
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
	 * Gets the full name.
	 *
	 * @return the fullName
	 */
	public String getFullName() {

		if (!ACMValidationUtils.isNullOrEmpty(this.groupes)) {
			fullName = this.prenom + " " + this.nom + " ("
					+ this.groupes.iterator().next().getLibelle() + ")";
		}
		else {
			fullName = this.prenom + " " + this.nom;
		}
		return fullName;
	}

	/**
	 * Sets the full name.
	 *
	 * @param fullName the fullName to set
	 */
	public void setFullName(String fullName) {

		this.fullName = fullName;
	}

	/**
	 * Gets the account portfolio id.
	 *
	 * @return the accountPortfolioId
	 */
	public Long getAccountPortfolioId() {

		return accountPortfolioId;
	}

	/**
	 * Sets the account portfolio id.
	 *
	 * @param accountPortfolioId the accountPortfolioId to set
	 */
	public void setAccountPortfolioId(Long accountPortfolioId) {

		this.accountPortfolioId = accountPortfolioId;
	}

	/**
	 * Gets the responsable id.
	 *
	 * @return the responsableId
	 */
	public String getResponsableId() {

		return responsableId;
	}

	/**
	 * Sets the responsable id.
	 *
	 * @param responsableId the responsableId to set
	 */
	public void setResponsableId(String responsableId) {

		this.responsableId = responsableId;
	}

	/**
	 * Gets the user extern id.
	 *
	 * @return the userExternId
	 */
	public Long getUserExternId() {

		return userExternId;
	}

	/**
	 * Sets the user extern id.
	 *
	 * @param userExternId the userExternId to set
	 */
	public void setUserExternId(Long userExternId) {

		this.userExternId = userExternId;
	}

	/**
	 * Gets the user profil id.
	 *
	 * @return the userProfilId
	 */
	public Long getUserProfilId() {

		return userProfilId;
	}

	/**
	 * Sets the user profil id.
	 *
	 * @param userProfilId the userProfilId to set
	 */
	public void setUserProfilId(Long userProfilId) {

		this.userProfilId = userProfilId;
	}

	/**
	 * Gets the groupes.
	 *
	 * @return the groupes
	 */
	public Set<GroupeDTO> getGroupes() {

		return groupes;
	}

	/**
	 * Sets the groupes.
	 *
	 * @param groupes the groupes to set
	 */
	public void setGroupes(Set<GroupeDTO> groupes) {

		this.groupes = groupes;
	}

	/**
	 * Gets the active.
	 *
	 * @return the active
	 */
	public Boolean getActive() {

		return active;
	}

	/**
	 * Sets the active.
	 *
	 * @param active the active to set
	 */
	public void setActive(Boolean active) {

		this.active = active;
	}

	/**
	 * Gets the new entry.
	 *
	 * @return the newEntry
	 */
	public Boolean getNewEntry() {

		return newEntry;
	}

	/**
	 * Sets the new entry.
	 *
	 * @param newEntry the newEntry to set
	 */
	public void setNewEntry(Boolean newEntry) {

		this.newEntry = newEntry;
	}

	/**
	 * Gets the type user : CONNECTED_USER / RESPONSIBLE / COLLABORATORS.
	 *
	 * @return the typeUser
	 */
	public String getTypeUser() {

		return typeUser;
	}

	/**
	 * Sets the type user : CONNECTED_USER / RESPONSIBLE / COLLABORATORS.
	 *
	 * @param typeUser the typeUser to set
	 */
	public void setTypeUser(String typeUser) {

		this.typeUser = typeUser;
	}

	/**
	 * Gets the branch ID.
	 * 
	 * @return the branchID
	 */
	public Integer getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 * 
	 * @param branchID the branchID to set
	 */
	public void setBranchID(Integer branchID) {

		this.branchID = branchID;
	}

	/**
	 * Gets the branch name.
	 * 
	 * @return the branchName
	 */
	public String getBranchName() {

		return branchName;
	}

	/**
	 * Sets the branch name.
	 * 
	 * @param branchName the branchName to set
	 */
	public void setBranchName(String branchName) {

		this.branchName = branchName;
	}

	/**
	 * Gets the branch description.
	 * 
	 * @return the branchDescription
	 */
	public String getBranchDescription() {

		return branchDescription;
	}

	/**
	 * Sets the branch description.
	 * 
	 * @param branchDescription the branchDescription to set
	 */
	public void setBranchDescription(String branchDescription) {

		this.branchDescription = branchDescription;
	}

	/**
	 * Gets the pwd.
	 *
	 * @return the pwd
	 */
	public String getPwd() {

		return pwd;
	}

	/**
	 * Sets the pwd.
	 *
	 * @param pwd the pwd to set
	 */
	public void setPwd(String pwd) {

		this.pwd = pwd;
	}

	/**
	 * Gets the pwd new.
	 *
	 * @return the pwdNew
	 */
	public String getPwdNew() {

		return pwdNew;
	}

	/**
	 * Sets the pwd new.
	 *
	 * @param pwdNew the pwdNew to set
	 */
	public void setPwdNew(String pwdNew) {

		this.pwdNew = pwdNew;
	}

	/**
	 * Gets the pwd confirm.
	 *
	 * @return the pwdConfirm
	 */
	public String getPwdConfirm() {

		return pwdConfirm;
	}

	/**
	 * Sets the pwd confirm.
	 *
	 * @param pwdConfirm the pwdConfirm to set
	 */
	public void setPwdConfirm(String pwdConfirm) {

		this.pwdConfirm = pwdConfirm;
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
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the access branches.
	 *
	 * @return the accessBranches
	 */
	public String getAccessBranches() {

		return accessBranches;
	}

	/**
	 * Sets the access branches.
	 *
	 * @param accessBranches the accessBranches to set
	 */
	public void setAccessBranches(String accessBranches) {

		this.accessBranches = accessBranches;
	}

	/**
	 * Gets the groupe code.
	 *
	 * @return the groupeCode
	 */
	public String getGroupeCode() {

		return groupeCode;
	}

	/**
	 * Sets the groupe code.
	 *
	 * @param groupeCode the groupeCode to set
	 */
	public void setGroupeCode(String groupeCode) {

		this.groupeCode = groupeCode;
	}

	/**
	 * Gets the simple name.
	 *
	 * @return the simpleName
	 */
	public String getSimpleName() {

		simpleName =
				(this.prenom != null ? this.prenom : "") + " " + (this.nom != null ? this.nom : "");
		return simpleName;
	}

	/**
	 * Sets the simple name.
	 *
	 * @param simpleName the simpleName to set
	 */
	public void setSimpleName(String simpleName) {

		this.simpleName = simpleName;
	}

	/**
	 * Gets the temporary pwd.
	 *
	 * @return the temporaryPwd
	 */
	public Boolean getTemporaryPwd() {

		return temporaryPwd;
	}

	/**
	 * Sets the temporary pwd.
	 *
	 * @param temporaryPwd the temporaryPwd to set
	 */
	public void setTemporaryPwd(Boolean temporaryPwd) {

		this.temporaryPwd = temporaryPwd;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the portfolio name.
	 *
	 * @return the portfolioName
	 */
	public String getPortfolioName() {

		return portfolioName;
	}

	/**
	 * Sets the portfolio name.
	 *
	 * @param portfolioName the portfolioName to set
	 */
	public void setPortfolioName(String portfolioName) {

		this.portfolioName = portfolioName;
	}

	/**
	 * Gets the default lang.
	 *
	 * @return the defaultLang
	 */
	public String getDefaultLang() {

		return defaultLang;
	}

	/**
	 * Sets the default lang.
	 *
	 * @param defaultLang the defaultLang to set
	 */
	public void setDefaultLang(String defaultLang) {

		this.defaultLang = defaultLang;
	}

	/**
	 * Gets the old responsible id.
	 *
	 * @return the old responsible id
	 */
	public String getOldResponsibleId() {

		return oldResponsibleId;
	}

	/**
	 * Sets the old responsible id.
	 *
	 * @param oldResponsibleId the new old responsible id
	 */
	public void setOldResponsibleId(String oldResponsibleId) {

		this.oldResponsibleId = oldResponsibleId;
	}

	/**
	 * Gets the hiring date.
	 *
	 * @return the hiring date
	 */
	public Date getHiringDate() {

		return hiringDate;
	}

	/**
	 * Sets the hiring date.
	 *
	 * @param hiringDate the new hiring date
	 */
	public void setHiringDate(Date hiringDate) {

		this.hiringDate = hiringDate;
	}

	/**
	 * Gets the resigning date.
	 *
	 * @return the resigning date
	 */
	public Date getResigningDate() {

		return resigningDate;
	}

	/**
	 * Sets the resigning date.
	 *
	 * @param resigningDate the new resigning date
	 */
	public void setResigningDate(Date resigningDate) {

		this.resigningDate = resigningDate;
	}

	/**
	 * Gets the old responsable name.
	 *
	 * @return the old responsable name
	 */
	public String getOldResponsableName() {

		return oldResponsableName;
	}

	/**
	 * Sets the old responsable name.
	 *
	 * @param oldResponsableName the new old responsable name
	 */
	public void setOldResponsableName(String oldResponsableName) {

		this.oldResponsableName = oldResponsableName;
	}

	/**
	 * Gets the temporary responsable.
	 *
	 * @return the temporary responsable
	 */
	public Boolean getTemporaryResponsable() {

		return temporaryResponsable;
	}

	/**
	 * Sets the temporary responsable.
	 *
	 * @param temporaryResponsable the new temporary responsable
	 */
	public void setTemporaryResponsable(Boolean temporaryResponsable) {

		this.temporaryResponsable = temporaryResponsable;
	}

	/**
	 * Gets the change all responsible.
	 *
	 * @return the change all responsible
	 */
	public Boolean getChangeAllResponsible() {

		return changeAllResponsible;
	}

	/**
	 * Sets the change all responsible.
	 *
	 * @param changeAllResponsible the new change all responsible
	 */
	public void setChangeAllResponsible(Boolean changeAllResponsible) {

		this.changeAllResponsible = changeAllResponsible;
	}

	/**
	 * Gets the login and name.
	 *
	 * @return the login and name
	 */
	public String getLoginAndName() {

		loginAndName = this.fullName + " " + this.login;

		return loginAndName;
	}

	/**
	 * Sets the login and name.
	 *
	 * @param loginAndName the new login and name
	 */
	public void setLoginAndName(String loginAndName) {

		this.loginAndName = loginAndName;
	}

	/**
	 * Gets the employee id.
	 *
	 * @return the employee id
	 */
	public String getEmployeeId() {

		return employeeId;
	}

	/**
	 * Sets the employee id.
	 *
	 * @param employeeId the new employee id
	 */
	public void setEmployeeId(String employeeId) {

		this.employeeId = employeeId;
	}
	
	/**
	 * Gets the pwd expiry date.
	 *
	 * @return the pwd expiry date
	 */
	public Date getPwdExpiryDate() {
	
		return pwdExpiryDate;
	}

	/**
	 * Sets the pwd expiry date.
	 *
	 * @param pwdExpiryDate the new pwd expiry date
	 */
	public void setPwdExpiryDate(Date pwdExpiryDate) {
	
		this.pwdExpiryDate = pwdExpiryDate;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return " { \"login\":\"" + login + "\",\"nom\":\"" + nom + "\", \"prenom\":\"" + prenom
				+ "\", \"email\":\"" + email + "\", \"fullName\":\"" + fullName
				+ "\", \"simpleName\":\"" + simpleName + "\",\"accountPortfolioId\":"
				+ accountPortfolioId + ", \"responsableId\":\"" + responsableId
				+ "\", \"userExternId\":" + userExternId + ",\"userProfilId\":" + userProfilId
				+ ", \"active\":\"" + active + "\", \"newEntry\":\"" + newEntry
				+ "\", \"typeUser\":\"" + typeUser + "\", \"branchID\":" + branchID
				+ ", \"branchName\":\"" + branchName + "\", \"branchDescription\":\""
				+ branchDescription + "\", \"pwd\":\"" + pwd + "\", \"pwdNew\":\"" + pwdNew
				+ "\", \"pwdConfirm\":\"" + pwdConfirm + "\", \"enabled\":\"" + enabled
				+ "\", \"customerId\":" + customerId + ", \"accessBranches\":\"" + accessBranches
				+ "\", \"groupeCode\":\"" + groupeCode + "\", \"temporaryPwd\":\"" + temporaryPwd
				+ "\", \"category\":\"" + category + "\", \"defaultLang\":\"" + defaultLang
				+ "\", \"groupes\":" + groupes + ", \"portfolioName\":\"" + portfolioName
				+ "\", \"oldResponsibleId\":\"" + oldResponsibleId + "\", \"hiringDate\":\""
				+ hiringDate + "\", \"resigningDate\":\"" + resigningDate
				+ "\", \"oldResponsableName\":\"" + oldResponsableName
				+ "\", \"temporaryResponsable\":\"" + temporaryResponsable
				+ "\", \"changeAllResponsible\":\"" + changeAllResponsible
				+ "\", \"loginAndName\":\"" + loginAndName + "\", \"employeeId\":\"" + employeeId
				+ "\"}";
	}

}
