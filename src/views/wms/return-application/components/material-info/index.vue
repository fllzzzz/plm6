<template>
  <component :is="comp" :basicClass="basicClass" :material="material" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoil
    case rawMatClsEnum.MATERIAL.V:
      return auxMat
    case rawMatClsEnum.GAS.V:
      return gas
    default:
      return auxMat
  }
})
</script>

<style lang="scss" scoped>
::v-deep(.important-info) {
  color: #e6a23c;
}

::v-deep(.returnable-number) {
  color: #67c23a;
}

.return-material-info {
  width: 100%;
  padding: 5px 0;
  border: 1px dashed #ccc;
  ::v-deep(.info-item) {
    display: inline-block;
    margin: 5px 0;
    font-size: 13px;
    min-width: 180px;
    // color: brown;
    // font-weight: bold;
    // width: 100px;
    > span {
      display: inline-block;
      overflow: hidden;
    }
    > span:first-child {
      font-weight: bold;
      width: 90px;
      text-align: right;
      &:after {
        content: '：';
      }
    }
    > span:last-child {
      text-overflow: ellipsis;
      white-space: nowrap;
      // display: inline;
      // color: #ccc;
      // font-weight: normal;
    }
  }
}

</style>
