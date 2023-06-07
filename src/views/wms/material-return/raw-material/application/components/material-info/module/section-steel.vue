<template>
  <div class="return-material-info" v-if="isNotBlank(material)">
    <span class="info-item">
      <span>编号</span>
      <span>{{ material.serialNumber }}</span>
    </span>
    <span class="info-item">
      <span>种类</span>
      <span>{{ material.classifyFullName }}</span>
    </span>
    <span class="info-item">
      <span>规格</span>
      <span>{{ material.specification }}</span>
    </span>
    <span class="info-item">
      <span>长度({{ baseUnit.length.unit }})</span>
      <span class="important-info" v-to-fixed="{ val: material.length || 0, dp: baseUnit.length.precision }" />
    </span>
    <span class="info-item">
      <span>品牌</span>
      <span>{{ material.brand }}</span>
    </span>
    <span class="info-item">
      <span>炉批号</span>
      <span>{{ material.heatNoAndBatchNo }}</span>
    </span>
    <span class="info-item">
      <span>单长度({{ baseUnit.length.unit }})</span>
      <span class="returnable-number" v-to-fixed="{ val: material.singleReturnableLength || 0, dp: baseUnit.length.precision }" />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.length || 0, dp: baseUnit.length.precision }" />
    </span>
    <span class="info-item">
      <span>总长度({{ baseUnit.length.unit }})</span>
      <span
        :style="{ color: materialReturnableLength < 0 ? 'red' : '#67c23a' }"
        v-to-fixed="{ val: materialReturnableLength || 0, dp: baseUnit.length.precision }"
      />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.totalLength || 0, dp: baseUnit.length.precision }" />
    </span>
    <br />
    <span class="info-item">
      <span>项目</span>
      <span v-parse-project="{ project: material.project, onlyShortName: true }" v-empty-text />
    </span>
    <span class="info-item">
      <span>车间</span>
      <span v-if="material.workshop">{{ material.workshop.name }}</span>
    </span>
    <span class="info-item">
      <span>仓库</span>
      <span v-if="material.warehouse">{{ material.warehouse.name }}</span>
    </span>
    <span class="info-item">
      <span>领用人</span>
      <span>{{ material.recipientName }}</span>
    </span>
    <span class="info-item">
      <span>出库日期</span>
      <span v-parse-time="{ val: material.createTime, fmt: '{y}-{m}-{d}' }" />
    </span>
    <span class="info-item">
      <span>数量({{ baseUnit.measure.unit }})</span>
      <span
        :style="{ color: materialReturnableQuantity < 0 ? 'red' : '#67c23a' }"
        v-to-fixed="{ val: materialReturnableQuantity || 0, dp: baseUnit.measure.precision }"
      />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.quantity || 0, dp: baseUnit.measure.precision }" />
    </span>
    <span class="info-item">
      <span>单重({{ baseUnit.weight.unit }})</span>
      <span class="returnable-number" v-to-fixed="{ val: material.singleReturnableMete || 0, dp: baseUnit.weight.precision }" />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.singleMete || 0, dp: baseUnit.weight.precision }" />
    </span>
    <span class="info-item">
      <span>总重({{ baseUnit.weight.unit }})</span>
      <span
        :style="{ color: materialReturnableMete < 0 ? 'red' : '#67c23a' }"
        v-to-fixed="{ val: materialReturnableMete || 0, dp: baseUnit.weight.precision }"
      />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.mete || 0, dp: baseUnit.weight.precision }" />
    </span>
  </div>
</template>

<script setup>
import { computed, defineProps } from 'vue'
import { isNotBlank, toPrecision } from '@/utils/data-type'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

// eslint-disable-next-line no-unused-vars
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

// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(props.basicClass)
// 退库量
const materialReturnableMete = computed(() => toPrecision(props.material.returnableMete, props.material.accountingPrecision))
// 退库数量
const materialReturnableQuantity = computed(() => toPrecision(props.material.returnableQuantity, props.material.measurePrecision))
// 退库长度
const materialReturnableLength = computed(() => toPrecision(props.material.returnableLength, baseUnit.value ? baseUnit.value.length.precision : 0))
</script>
