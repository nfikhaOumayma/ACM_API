/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserLangValues;
 
/**
 * {@link User} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_USERS")
@NamedQuery(name = "User.findAll", query = "SELECT u FROM User u")
public class User extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1083246912484526667L;

	/** The username. */
	@Id
	@Column(name = "USERNAME", unique = true, nullable = false, length = 256)
	private String username;

	/** The account portfolio id. */
	@Column(name = "ACCOUNT_PORTFOLIO_ID", nullable = false)
	private Long accountPortfolioId;

	/** The responsable id. */
	@Column(name = "RESPONSABLE_ID")
	private String responsableId;

	/** The user extern id. */
	@Column(name = "USER_EXTERN_ID", nullable = false)
	private Long userExternId;

	/** The user profil id. */
	@Column(name = "USER_PROFIL_ID", nullable = false)
	private Long userProfilId;

	/** The password. */
	@Column(name = "PASSWORD", nullable = false, length = 256)
	private String password;

	/** The nom. */
	@Column(name = "SUR_NAME")
	private String nom;

	/** The prenom. */
	@Column(name = "NAME")
	private String prenom;

	/** The email. */
	@Column(name = "EMAIL")
	private String email;

	/** The branchID. */
	@Column(name = "BRANCHID")
	private Integer branchID;

	/** The branch name. */
	@Column(name = "BRANCHE_NAME", length = 50)
	private String branchName;

	/** The branch description. */
	@Column(name = "BRANCHE_DESCRIPTION", length = 200)
	private String branchDescription;

	/** The id customer. */
	@Column(name = "CUSTOMER_ID")
	private Long customerId;

	/** The access branches. */
	@Column(name = "ACCESS_BRANCHES")
	private String accessBranches;

	/** The temporary pwd. */
	@Column(name = "TEMPORARY_PWD")
	private Boolean temporaryPwd;

	/** The category : {@link UserCategory} ENUMS. */
	@Column(name = "CATEGORY")
	private String category;

	/** The defaultLang : {@link UserLangValues} ENUMS. */
	@Column(name = "DEFAULT_LANG")
	private String defaultLang;

	/** The token. */
	@Transient
	private String token;

	/** The portfolio name. */
	@Column(name = "PORTFOLIO_NAME")
	private String portfolioName;

	/** The groupes. */
	@ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinTable(name = "ACM_USERS_GROUPE", joinColumns = {@JoinColumn(name = "USERNAME")},
			inverseJoinColumns = {@JoinColumn(name = "ID_ACM_GROUPE")})
	private Set<Groupe> groupes = new HashSet<>();

	/** The hiring date. */
	@Column(name = "HIRING_DATE")
	private Date hiringDate;

	/** The resigning date. */
	@Column(name = "RESIGNING_DATE")
	private Date resigningDate;

	/** The temporary responsable. */
	@Column(name = "TEMPORARY_RESPONSABLE")
	private Boolean temporaryResponsable;

	/** The old responsible id. */
	@Column(name = "OLD_RESPONSABLE_ID")
	private String oldResponsibleId;

	/** The old responsable name. */
	@Column(name = "OLD_RESPONSABLE_NAME")
	private String oldResponsableName;

	/** The employee id. */
	@Column(name = "EMPLOYEE_ID")
	private String employeeId;

	/** The failed attempts. */
	@Column(name = "FAILED_ATTEMPTS")
	private Integer failedAttempts;

	/** The pwd expiry date. */
	@Column(name = "PWD_EXPIRY_DATE")
	private Date pwdExpiryDate;

	/** The acm conditionnal approves. */
	@OneToMany(mappedBy = "user", fetch = FetchType.LAZY, orphanRemoval = true)
	private Set<AcmConditionnalApprove> acmConditionnalApproves = new HashSet<>();

	/**
	 * Instantiates a new user.
	 */
	public User() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param username the username
	 */
	public User(String username) {

		this.username = username;
	}

	/**
	 * Instantiates a new user(USED in BATCH).
	 *
	 * @param username the username
	 * @param accountPortfolioId the account portfolio id
	 * @param userExternId the user extern id
	 * @param userProfilId the user profil id
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param token the token
	 */
	public User(String username, Long accountPortfolioId, Long userExternId, Long userProfilId,
			Integer branchID, String branchName, String branchDescription, String token) {

		this.username = username;
		this.accountPortfolioId = accountPortfolioId;
		this.userExternId = userExternId;
		this.userProfilId = userProfilId;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
		this.token = token;
	}

	/**
	 * Instantiates a new user(USED in BATCH).
	 *
	 * @param username the username
	 * @param accountPortfolioId the account portfolio id
	 * @param userExternId the user extern id
	 * @param userProfilId the user profil id
	 * @param branchID the branch ID
	 * @param branchName the branch name
	 * @param branchDescription the branch description
	 * @param token the token
	 * @param portfolioName the portfolio name
	 */
	public User(String username, Long accountPortfolioId, Long userExternId, Long userProfilId,
			Integer branchID, String branchName, String branchDescription, String token,
			String portfolioName) {

		this.username = username;
		this.accountPortfolioId = accountPortfolioId;
		this.userExternId = userExternId;
		this.userProfilId = userProfilId;
		this.branchID = branchID;
		this.branchName = branchName;
		this.branchDescription = branchDescription;
		this.token = token;
		this.portfolioName = portfolioName;
	}

	/**
	 * Gets the portfolio name.
	 *
	 * @return the portfolio name
	 */
	public String getPortfolioName() {

		return portfolioName;
	}

	/**
	 * Sets the portfolio name.
	 *
	 * @param portfolioName the new portfolio name
	 */
	public void setPortfolioName(String portfolioName) {

		this.portfolioName = portfolioName;
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
	 * @param email the email to set
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the groupes.
	 *
	 * @return the groupes
	 */
	public Set<Groupe> getGroupes() {

		return groupes;
	}

	/**
	 * Sets the groupes.
	 *
	 * @param groupes the groupes to set
	 */
	public void setGroupes(Set<Groupe> groupes) {

		this.groupes = groupes;
	}

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUsername() {

		return username;
	}

	/**
	 * Sets the username.
	 *
	 * @param username the username to set
	 */
	public void setUsername(String username) {

		this.username = username;
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
	 * Gets the password.
	 *
	 * @return the password
	 */
	public String getPassword() {

		return password;
	}

	/**
	 * Sets the password.
	 *
	 * @param password the password to set
	 */
	public void setPassword(String password) {

		this.password = password;
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
	 * Gets the token.
	 *
	 * @return the token
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the token to set
	 */
	public void setToken(String token) {

		this.token = token;
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
	 * Gets the failed attempts.
	 * 
	 * @return the failed attempts
	 */
	public Integer getFailedAttempts() {

		return failedAttempts;
	}

	/**
	 * Sets the failed attempts.
	 *
	 * @param failedAttempts the failed attempts
	 */
	public void setFailedAttempts(Integer failedAttempts) {

		this.failedAttempts = failedAttempts;
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
	 * @see com.acm.utils.models.GenericModel#toString()
	 */
	@Override
	public String toString() {

		return "User [" + (username != null ? "username=" + username + ", " : "")
				+ (accountPortfolioId != null ? "accountPortfolioId=" + accountPortfolioId + ", "
						: "")
				+ (responsableId != null ? "responsableId=" + responsableId + ", " : "")
				+ (userExternId != null ? "userExternId=" + userExternId + ", " : "")
				+ (userProfilId != null ? "userProfilId=" + userProfilId + ", " : "")
				+ (password != null ? "password=" + password + ", " : "")
				+ (nom != null ? "nom=" + nom + ", " : "")
				+ (prenom != null ? "prenom=" + prenom + ", " : "")
				+ (email != null ? "email=" + email + ", " : "")
				+ (branchID != null ? "branchID=" + branchID + ", " : "")
				+ (branchName != null ? "branchName=" + branchName + ", " : "")
				+ (branchDescription != null ? "branchDescription=" + branchDescription + ", " : "")
				+ (customerId != null ? "customerId=" + customerId + ", " : "")
				+ (accessBranches != null ? "accessBranches=" + accessBranches + ", " : "")
				+ (temporaryPwd != null ? "temporaryPwd=" + temporaryPwd + ", " : "")
				+ (category != null ? "category=" + category + ", " : "")
				+ (defaultLang != null ? "defaultLang=" + defaultLang + ", " : "")
				+ (token != null ? "token=" + token + ", " : "")
				+ (portfolioName != null ? "portfolioName=" + portfolioName + ", " : "")
				+ (groupes != null ? "groupes=" + groupes + ", " : "")
				+ (hiringDate != null ? "hiringDate=" + hiringDate + ", " : "")
				+ (resigningDate != null ? "resigningDate=" + resigningDate : "") + "]";
	}

}
