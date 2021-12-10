<template>
  <el-table-column v-if="showIndex" label="序号" type="index" align="center" width="55" fixed="left" >
    <template #default="{ row, $index }">
      <table-cell-tag v-if="showPartyA" :show="!!row.boolPartyA" name="甲供" :color="TAG_PARTY_DEF_COLOR" />
      <span>{{ $index + 1 }}</span>
    </template>
  </el-table-column>
  <component :is="comp" :columns="props.columns" :basic-class="props.basicClass" :spec-merge="props.specMerge" :show-factory="props.showFactory" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { TAG_PARTY_DEF_COLOR } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import TableCellTag from '@/components-system/common/table-cell-tag/index.vue'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import rawMat from './module/raw-mat.vue'

const props = defineProps({
  specMerge: { // 规格合并,规格与 厚宽长颜色等合并为一个字段
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  showFactory: { // 显示 “工厂”
    type: Boolean,
    default: false
  },
  showIndex: { // 显示 “序号”
    type: Boolean,
    default: true
  },
  showPartyA: { // 显示 “甲供”
    type: Boolean,
    default: true
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V: return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V: return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V: return steelCoil
    case rawMatClsEnum.MATERIAL.V: return auxMat
    case rawMatClsEnum.GAS.V: return gas
    default: return rawMat
  }
})

</script>
