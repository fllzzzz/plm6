<template>
  <el-table-column
    v-if="showSpecification && specMerge"
    key="specificationMerge"
    prop="specificationMerge"
    label="规格"
    min-width="200"
    align="center"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <el-tooltip :content="specTip(row)" placement="left">
        <span>{{ specFormat(row) }}</span>
      </el-tooltip>
    </template>
  </el-table-column>
  <template v-if="specOnly">
    <el-table-column
      v-if="showSpecification"
      key="specification_2"
      prop="specification"
      :label="specOnly?'材质':'规格'"
      align="center"
      width="250px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <template v-if="[rawMatClsEnum.STEEL_PLATE.V, rawMatClsEnum.SECTION_STEEL.V,rawMatClsEnum.STEEL_COIL.V].includes(row.basicClass)">
          <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="left">
            {{ row.specification }}
          </el-tooltip>
        </template>
        <template v-else>
           <el-tooltip :content="otherTip(row)" placement="left">
            <span>{{ otherFormat(row) || '-' }}</span>
          </el-tooltip>
        </template>
      </template>
    </el-table-column>
     <el-table-column
        v-if="showOtherMerge"
        key="otherMerge"
        prop="otherMerge"
        label="规格"
        align="center"
        width="200px"
        :fixed="fixed"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <template
            v-if="[rawMatClsEnum.STEEL_PLATE.V,rawMatClsEnum.SECTION_STEEL.V,rawMatClsEnum.STEEL_COIL.V].includes(row.basicClass)">
            <el-tooltip :content="otherTip(row)" placement="left">
              <span>{{ otherFormat(row) || '-' }}</span>
            </el-tooltip>
          </template>
          <template v-else>
            <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="left">
              {{ row.specification }}
            </el-tooltip>
          </template>
        </template>
      </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import { otherFormat, otherTip } from '@/utils/wms/other-format'

const props = defineProps({
  specMerge: {
    type: Boolean,
    default: false
  },
  specOnly: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  },
  sortable: {
    type: [String, Boolean],
    default: false
  }
})

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showOtherMerge = computed(() => isBlank(props.columns) || props.columns.visible('otherMerge'))
</script>
