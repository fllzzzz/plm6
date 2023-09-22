<template>
  <common-dialog :title="`${packageInfo.serialNumber}`" top="5vh" v-model="dialogVisible" width="600px" :before-close="handleClose">
    <div style="font-weight: bold; font-size: 16pt; color: #333; padding-bottom: 10pt">{{ packageInfo.project.name }}</div>
    <table v-if="packageInfo.productType !== 10" border="1" bordercolor="#000000">
      <tr>
        <td rowspan="2">
          <div class="qr-content">
            <qrcode-vue :value="labelData.qrCode" :size="90" :margin="2" />
          </div>
        </td>
        <td :colspan="packageInfo.productType === packTypeEnum.ENCLOSURE.V ? 4 : 3">
          <span style="font-weight: bold; font-size: 10pt">打包单</span>
        </td>
      </tr>
      <tr>
        <td>包单号</td>
        <td :colspan="packageInfo.productType === packTypeEnum.ENCLOSURE.V ? 3 : 2" style="font-weight: bold">{{ packageInfo.serialNumber }}</td>
      </tr>
      <tr v-if="packageInfo.productType !== packTypeEnum.AUXILIARY_MATERIAL.V && packageInfo.productType !== packTypeEnum.ENCLOSURE.V">
        <td>编号</td>
        <td>材质</td>
        <td>数量</td>
        <td>重量(kg)</td>
      </tr>
      <tr v-if="packageInfo.productType === packTypeEnum.AUXILIARY_MATERIAL.V">
        <td>名称</td>
        <td v-if="props.unitType === 1">核算单位</td>
        <td v-else>计量单位</td>
        <td>规格</td>
        <td v-if="props.unitType === 1">核算量</td>
        <td v-else>数量</td>
      </tr>
      <tr v-if="packageInfo.productType === packTypeEnum.ENCLOSURE.V">
        <td>编号</td>
        <td>板型</td>
        <td>单长(mm)</td>
        <td>单面积(mm²)</td>
        <td>数量</td>
      </tr>
      <template
        v-if="packageInfo.productType !== packTypeEnum.AUXILIARY_MATERIAL.V && packageInfo.productType !== packTypeEnum.ENCLOSURE.V"
      >
        <tr v-for="(item, index) in breakUpList[page - 1]" :key="index">
          <td class="col-1">{{ item.serialNumber }}</td>
          <td class="col-1">{{ item.material }}</td>
          <td class="col-1">{{ item.quantity }}</td>
          <td class="col-1">{{ item.totalWeight }}</td>
        </tr>
      </template>
      <template v-if="packageInfo.productType === packTypeEnum.AUXILIARY_MATERIAL.V">
        <tr v-for="(item, index) in breakUpList[page - 1]" :key="index">
          <td class="col-1">{{ item.name }}</td>
          <td class="col-1" v-if="props.unitType === 1">{{ item.accountingUnit }}</td>
          <td class="col-1" v-else>{{ item.measureUnit }}</td>
          <td class="col-1">{{ item.specification }}</td>
          <td class="col-1" v-if="props.unitType === 1">{{ item.packageMete }}</td>
          <td class="col-1" v-else>{{ item.quantity }}</td>
        </tr>
      </template>
      <template v-if="packageInfo.productType === packTypeEnum.ENCLOSURE.V">
        <tr v-for="(item, index) in breakUpList[page - 1]" :key="index">
          <td class="col-1">{{ item.serialNumber }}</td>
          <td class="col-1">{{ item.plate }}</td>
          <td class="col-1">{{ item.length }}</td>
          <td class="col-1">{{ item.surfaceArea }}</td>
          <td class="col-1">{{ item.quantity }}</td>
        </tr>
      </template>
    </table>
    <table v-if="packageInfo.productType === 10" border="1" bordercolor="#000000">
      <tr>
        <td rowspan="2">
          <div class="qr-content">
            <qrcode-vue :value="labelData.qrCode" :size="90" :margin="2" />
          </div>
        </td>
        <td colspan="3">
          <span style="font-weight: bold; font-size: 10pt">打包单</span>
        </td>
      </tr>
      <tr>
        <td>包单号</td>
        <td colspan="2" style="font-weight: bold">{{ packageInfo.serialNumber }}</td>
      </tr>
      <tr>
        <td colspan="4" style="font-weight: bold">结构</td>
      </tr>
      <tr>
        <td>编号</td>
        <td>材质</td>
        <td>数量</td>
        <td>重量(kg)</td>
      </tr>
      <!-- <template> -->
      <tr v-for="(item, index) in structureData" :key="index">
        <td class="col-1">{{ item.serialNumber }}</td>
        <td class="col-1">{{ item.material }}</td>
        <td class="col-1">{{ item.quantity }}</td>
        <td class="col-1">{{ item.totalWeight }}</td>
      </tr>
      <!-- </template> -->
      <tr>
        <td colspan="4" style="font-weight: bold">辅材</td>
      </tr>
      <tr>
        <td>名称</td>
        <td v-if="props.unitType === 1">核算单位</td>
        <td v-else>计量单位</td>
        <td>规格</td>
        <td v-if="props.unitType === 1">核算量</td>
        <td v-else>数量</td>
      </tr>
      <!-- <tr v-if="packageInfo.productType === packTypeEnum.ENCLOSURE.V">
        <td>编号</td>
        <td>板型</td>
        <td>长度</td>
        <td>数量</td>
      </tr> -->

      <!-- <template> -->
      <tr v-for="(aux, index) in auxData" :key="index">
        <td class="col-1">{{ aux.name }}</td>
        <td class="col-1">{{ aux.measureUnit }}</td>
        <td class="col-1">{{ aux.specification }}</td>
        <td class="col-1">{{ aux.quantity }}</td>
      </tr>
      <!-- </template> -->
    </table>
    <el-pagination
      style="text-align: center; margin-top: 10px"
      small
      layout="prev, pager, next"
      :page-count="breakUpList.length"
      v-model:current-page="page"
    ></el-pagination>
  </common-dialog>
</template>

<script setup>
import QrcodeVue from 'qrcode.vue'
import { computed, ref, defineEmits, defineProps } from 'vue'
import { packTypeEnum } from '@enum-ms/mes'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['saveSuccess', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  labelData: {
    type: Object,
    default: () => {
      return {}
    }
  },
  unitType: {
    type: Number,
    default: 1
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const packageInfo = computed(() => {
  return props.labelData.packageInfo || {}
})
const page = ref(1)
const breakUpList = computed(() => {
  const _list = []
  for (var i = 0, len = packageInfo.value.list?.length; i < len; i += 11) {
    _list.push(packageInfo.value.list.slice(i, Math.min(i + 11, len)))
  }
  console.log(packageInfo.value)
  console.log(_list)
  return _list
})
const structureData = computed(() => {
  const _structureData = []
  for (var i = 0, len = packageInfo.value.structureList?.length; i < len; i++) {
    _structureData.push(packageInfo.value.structureList[i])
  }
  return _structureData
})
const auxData = computed(() => {
  const _auxData = []
  for (var i = 0, len = packageInfo.value.auxList?.length; i < len; i++) {
    _auxData.push(packageInfo.value.auxList[i])
  }
  return _auxData
})
</script>

<style lang="scss" scoped>
.qr-content {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
}
table {
  font-family: '微软雅黑';
  border-collapse: collapse;
  border: solid 1px;
  text-align: center;
  font-size: 9pt;
  color: black;
  tr td {
    padding: 0 10px;
    height: 50px;
    width: 150px;
    word-break: break-all;
  }
}
</style>
