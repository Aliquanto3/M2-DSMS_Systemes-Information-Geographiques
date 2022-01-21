# imports
from PyQt5.QtCore import QVariant

# accessing point layer by name
layer = QgsProject.instance().mapLayersByName('PPV_Centroid')[0]

if not layer.isValid():
    print("Layer failed to load!")

layer_provider = layer.dataProvider()

# adding new fields
for attr in ["X_Coord", "Y_Coord"]:
  layer_provider.addAttributes([QgsField(attr, QVariant.Double)])
layer.updateFields()

# starting layer editing
layer.startEditing()

for feature in layer.getFeatures():
    fields = layer.fields() # accessing layer fields
    attrs = {
            fields.indexFromName("X_Coord"): feature.geometry().asPoint()[0],
            fields.indexFromName("Y_Coord"): feature.geometry().asPoint()[1]
            }
    layer_provider.changeAttributeValues({feature.id(): attrs})

layer.commitChanges()

###########
# accessing point layer by name
layer = QgsProject.instance().mapLayersByName('TIN_Centroid')[0]

if not layer.isValid():
    print("Layer failed to load!")

layer_provider = layer.dataProvider()

# adding new fields
for attr in ["X_Coord", "Y_Coord"]:
  layer_provider.addAttributes([QgsField(attr, QVariant.Double)])
layer.updateFields()

# starting layer editing
layer.startEditing()

for feature in layer.getFeatures():
    fields = layer.fields() # accessing layer fields
    attrs = {
            fields.indexFromName("X_Coord"): feature.geometry().asPoint()[0],
            fields.indexFromName("Y_Coord"): feature.geometry().asPoint()[1]
            }
    layer_provider.changeAttributeValues({feature.id(): attrs})

layer.commitChanges()

###########
# accessing point layer by name
layer = QgsProject.instance().mapLayersByName('IDW_Centroid')[0]

if not layer.isValid():
    print("Layer failed to load!")

layer_provider = layer.dataProvider()

# adding new fields
for attr in ["X_Coord", "Y_Coord"]:
  layer_provider.addAttributes([QgsField(attr, QVariant.Double)])
layer.updateFields()

# starting layer editing
layer.startEditing()

for feature in layer.getFeatures():
    fields = layer.fields() # accessing layer fields
    attrs = {
            fields.indexFromName("X_Coord"): feature.geometry().asPoint()[0],
            fields.indexFromName("Y_Coord"): feature.geometry().asPoint()[1]
            }
    layer_provider.changeAttributeValues({feature.id(): attrs})

layer.commitChanges()