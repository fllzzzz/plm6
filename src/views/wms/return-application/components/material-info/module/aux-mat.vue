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
      <span>颜色</span>
      <span>{{ material.color }}</span>
    </span>
    <span class="info-item">
      <span>品牌</span>
      <span>{{ material.brand }}</span>
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
    <template v-if="material.measureUnit">
      <span class="info-item">
        <span> 单位净量 </span>
        <span
          class="returnable-number"
          v-to-fixed="{
            val: material.outboundUnitType === measureTypeEnum.MEASURE.V ? material.unitNet : material.accountingUnitNet,
            dp: material.outboundUnitType === measureTypeEnum.MEASURE.V ? material.accountingPrecision : material.measurePrecision
          }"
        />&nbsp;
        <span class="important-info">
          {{
            material.outboundUnitType === measureTypeEnum.MEASURE.V
              ? `${material.accountingUnit} / ${material.measureUnit}`
              : `${material.measureUnit} / ${material.accountingUnit}`
          }}
        </span>
      </span>
      <span class="info-item">
        <span>数量({{ material.measureUnit }})</span>
        <span class="returnable-number" v-to-fixed="{ val: material.quantity || 0, dp: material.measurePrecision }" />
      </span>
    </template>
    <span class="info-item">
      <span>核算量({{ material.accountingUnit }})</span>
      <span
        :style="{ color: material.returnableMete < 0 ? 'red' : '#67c23a' }"
        v-to-fixed="{ val: material.returnableMete || 0, dp: material.accountingPrecision }"
      />
      <span>&nbsp;/&nbsp;</span>
      <span v-to-fixed="{ val: material.mete || 0, dp: material.accountingPrecision }" />
    </span>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'
import { isNotBlank } from '@/utils/data-type'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
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
</script>
