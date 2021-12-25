<template>
  <div class="return-material-info" v-if="isNotBlank(material)">
    <span class="info-item">
      <span class="label">编号</span>
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
      <span>厚度(mm)</span>
      <span class="important-info">{{ material.thickness }}</span>
    </span>
    <span class="info-item">
      <span>宽度(mm)</span>
      <span class="important-info">{{ material.width }}</span>
    </span>
    <span class="info-item">
      <span>长度(mm)</span>
      <span class="important-info">{{ material.length }}</span>
    </span>
    <span class="info-item">
      <span>品牌</span>
      <span>{{ material.brand }}</span>
    </span>
    <span class="info-item">
      <span>炉批号</span>
      <span>{{ material.heatNoAndBatchNo }}</span>
    </span>
    <br />
    <span class="info-item">
      <span>项目</span>
      <span v-parse-project="{ project: material.project, onlyShortName: true }" v-empty-text />
    </span>
    <span class="info-item">
      <span>工厂</span>
      <span v-if="material.factory">{{ material.factory.name }}</span>
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
      <span v-parse-time="'{y}-{m}-{d}'">{{ material.createTime }}</span>
    </span>
    <span class="info-item">
      <span>数量({{ baseUnit.measure.unit }})</span>
      <span class="returnable-number">{{ material.quantity }}</span>
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
        :style="{ color: material.returnableMete < 0 ? 'red' : '#67c23a' }"
        v-to-fixed="{ val: material.returnableMete || 0, dp: baseUnit.weight.precision }"
      />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.mete || 0, dp: baseUnit.weight.precision }" />
    </span>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'
import { isNotBlank } from '@/utils/data-type'
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
</script>
