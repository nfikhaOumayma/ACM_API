//
// Ce fichier a été généré par l'implémentation de référence JavaTM Architecture for XML Binding (JAXB), v2.3.0 
// Voir <a href="https://javaee.github.io/jaxb-v2/">https://javaee.github.io/jaxb-v2/</a> 
// Toute modification apportée à ce fichier sera perdue lors de la recompilation du schéma source. 
// Généré le : 2022.03.17 à 05:09:34 PM GMT+01:00
//


package com.acm.soap.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour anonymous complex type.
 * 
 * <p>Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="PostRequestResult" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "postRequestResult"
})
@XmlRootElement(name = "PostRequestResponse")
public class PostRequestResponse {

    @XmlElement(name = "PostRequestResult")
    protected String postRequestResult;

    /**
     * Obtient la valeur de la propriété postRequestResult.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostRequestResult() {
        return postRequestResult;
    }

    /**
     * Définit la valeur de la propriété postRequestResult.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostRequestResult(String value) {
        this.postRequestResult = value;
    }

}
