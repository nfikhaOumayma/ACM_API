/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.constants.common;

/**
 * {@link CommonConstantGED} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public final class CommonConstantGED implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2785393365728106918L;

	/** The site to upload the file to. */
	public static final String ACM_SITE = "acm";

	/** Prod Config. */
	/** The Constant ACM_GED_URL. */
	// public static final String ACM_GED_URL = "http://dms.alahlytamkeen.net:8080";
	// /** The Constant DEFAULT_USERNAME. */
	// public static final String DEFAULT_USERNAME = "talys";
	// /** The Constant DEFAULT_PASSWORD. */
	// public static final String DEFAULT_PASS_GED = "Talys@Tamkeen";
	/**
	 * UAT Config
	 */
	/** The Constant ACM_GED_URL. */
	// public static final String ACM_GED_URL = "http://dms-uat.alahlytamkeen.net:8080";
	// /** The Constant DEFAULT_USERNAME. */
	// public static final String DEFAULT_USERNAME = "talys";
	// /** The Constant DEFAULT_PASSWORD. */
	// public static final String DEFAULT_PASS_GED = "Talys@Tamkeen";

	/** The Constant ACM_GED_URL. */
	public static final String ACM_GED_URL = "http://172.16.4.101:8080";
	/** The Constant DEFAULT_USERNAME. */
	public static final String DEFAULT_USERNAME = "acm_ged";
	/** The Constant DEFAULT_PASSWORD. */
	public static final String DEFAULT_PASS_GED = "acm_ged";
	/** The Constant DEFAULT_MIMETYPE. */
	public static final String DEFAULT_MIMETYPE = "application/octet-stream";
	/** The Constant PARAMETRAGE_CATEGORIE_USER. */
	public static final String PARAMETRAGE_CATEGORIE_USER = "USER";
	/** The Constant PARAMETRAGE_CATEGORIE_APPLICATION. */
	public static final String PARAMETRAGE_CATEGORIE_APPLICATION = "APPLICATION";
	/** The document library. */
	public static final String DOCUMENT_LIBRARY = "documentLibrary/";
	/** The Constant AUTHORIZATION. */
	public static final String AUTHORIZATION = "Authorization";
	/** The Constant BASIC. */
	public static final String BASIC = "Basic ";

	/** The uri nodes. */
	public static final String URI_GED_NODES =
			"/alfresco/api/-default-/public/alfresco/versions/1/nodes/";
	/** The uri token. */
	public static final String URI_GED_TOKEN =
			"/alfresco/api/-default-/public/authentication/versions/1/tickets";
	/** The uri search node. */
	public static final String URI_GED_SEARCH_NODE =
			"/alfresco/api/-default-/public/alfresco/versions/1/queries/nodes/?";
	/** The atom URL. */
	public static final String ATOMPUB_GED_URL =
			"/alfresco/api/-default-/public/cmis/versions/1.1/atom";
	/** The uri search api. */
	public static final String URI_GED_SEARCH_API =
			"/alfresco/api/-default-/public/search/versions/1/search";

	/** The Constant CMIS_DOCUMENT. */
	public static final String CMIS_DOCUMENT = "cmis:document";
	/** The Constant CMIS_OBJECT_ID. */
	public static final String CMIS_OBJECT_ID = "cmis:objectId";
	/** The Constant QUERY_FIND_DOCUMENT. */
	public static final String QUERY_FIND_DOCUMENT =
			"select * from st:site as s join cm:titled as t on s.cmis:objectId=t.cmis:objectId where s.cmis:name='%s'";

	/** The Constant MAX_ITEMS_VALUE. */
	public static final String MAX_ITEMS_VALUE = " 5";

	/** The Constant TERM_EQUAL. */
	public static final String TERM_EQUAL = "term=";
	/** The Constant CREATED_AT. */
	public static final String CREATED_AT = "createdAt";
	/** The Constant IS_FOLDER. */
	public static final String IS_FOLDER = "isFolder";
	/** The Constant IS_FILE. */
	public static final String IS_FILE = "isFile";
	/** The Constant MODIFIED_AT. */
	public static final String MODIFIED_AT = "modifiedAt";
	/** The Constant NAME. */
	public static final String NAME = "name";
	/** The Constant ID. */
	public static final String ID = "id";
	/** The Constant PARENT_ID. */
	public static final String PARENT_ID = "parentId";
	/** The Constant USER_ID. */
	public static final String USER_ID = "userId";
	/** The Constant TOKEN_PASS. */
	public static final String TOKEN_PASS = "password";

	/** The Constant LIST. */
	public static final String LIST = "list";
	/** The Constant ENTRIES. */
	public static final String ENTRIES = "entries";
	/** The Constant ENTRY. */
	public static final String ENTRY = "entry";
	/** The Constant CONTENT. */
	public static final String CONTENT = "content";
	/** The Constant MIME_TYPE. */
	public static final String MIME_TYPE = "mimeType";
	/** The Constant APPLICATION_JSON. */
	public static final String APPLICATION_JSON = "application/json";
	/** The Constant CONTENT_TYPE. */
	public static final String CONTENT_TYPE = "Content-Type";

	/** The Constant T. */
	public static final String T = "T";
	/** The Constant TIR_6. */
	public static final String TIR_6 = "-";
	/** The Constant SLASH_PATH. */
	public static final String SLASH_PATH = "/";
	/** The Constant SETTING DOC TYPE CODE PHOTO . */
	public static final String SETTING_DOC_TYPE_CODE_PHOTO = "PHOTO";
	/** The Constant CUSTOMER . */
	public static final String CUSTOMER = "CUSTOMER";
	/** The Constant LOGO. */
	public static final String LOGO = "LOGO";
	/*
	 * ### GED PATH
	 */
	/** The Constant GED_PATH_PRODUCTION_ACM_LOAN. */
	public static final String GED_PATH_PRODUCTION_ACM_LOAN = "acm_loan";
	/** The Constant GED_PATH_PRODUCTION_ACM_CUSTOMER. */
	public static final String GED_PATH_PRODUCTION_ACM_CUSTOMER = "acm_customer";

}
