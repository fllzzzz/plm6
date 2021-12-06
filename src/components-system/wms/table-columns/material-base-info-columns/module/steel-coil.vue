<template>
  <el-table-column  v-if="showSerialNumber" prop="serialNumber" label="编号" align="center" width="110px" fixed="left" >
    <template #default="{ row }">
      <factory-table-cell-tag v-if="props.showFactory" :id="row.factory ? row.factory.id : row.factoryId" />
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column  v-if="showClassifyFullName" prop="classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
  <template v-if="props.specMerge">
    <el-table-column  v-if="showSpecification" prop="specification" label="规格" align="center" width="220px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span v-empty-text>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column  v-if="showSpecification" prop="specification" label="规格" align="center" width="100px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span v-empty-text>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column  v-if="showThickness" prop="thickness" align="center" width="100px" :label="`厚 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ row.thickness }}</span>
      </template>
    </el-table-column>
    <el-table-column  v-if="showWidth" prop="width" align="center" width="120px" :label="`宽 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ row.width }}</span>
      </template>
    </el-table-column>
    <!-- <el-table-column v-if="showLength" prop="length" align="center" width="120px" :label="`长 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ row.length }}</span>
      </template>
    </el-table-column> -->
    <el-table-column  v-if="showColor" prop="color" align="center" width="120px" :label="`颜色`" />
  </template>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'

const props = defineProps({
  specMerge: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  showFactory: {
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  }
})

const showSerialNumber = computed(() => isBlank(props.columns) || props.columns.visible('serialNumber'))
const showClassifyFullName = computed(() => isBlank(props.columns) || props.columns.visible('classifyFullName'))
const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showThickness = computed(() => isBlank(props.columns) || props.columns.visible('thickness'))
const showWidth = computed(() => isBlank(props.columns) || props.columns.visible('width'))
// const showLength = computed(() => isBlank(props.columns) || props.columns.visible('length'))
const showColor = computed(() => isBlank(props.columns) || props.columns.visible('color'))
</script>
