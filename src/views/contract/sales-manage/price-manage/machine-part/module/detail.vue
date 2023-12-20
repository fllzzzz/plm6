<template>
  <common-drawer
  :visible="crud.detailVisible"
  :title="`散发制品详情`"
  size="70%"
  :before-close="handleClose"
  >
    <template #titleAfter>
      <el-tag type="warning" size="medium" effect="plain">综合单价：<span>{{ props.detailInfo?.totalPrice ? props.detailInfo?.totalPrice : '-' }}</span> </el-tag>
    </template>
    <template #titleRight>
      <print-table v-permission="crud.permission.print" api-key="machinePartDetail" :params="props.detailInfo.id" size="mini"></print-table>
    </template>
    <template #content>
      <common-table  :data="detail.content">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="name" label="名称" min-width="100px" align="center" />
        <el-table-column prop="serialNumber" label="编号" min-width="100px" align="center" />
        <el-table-column prop="specification" label="规格" min-width="100px" align="center" />
        <el-table-column prop="material" label="材质" min-width="100px" align="center" />
        <el-table-column prop="quantity" label="数量" min-width="80px" align="center" />
        <el-table-column prop="netWeight" label="单净重(kg)" min-width="100px" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps } from 'vue'
import { regDetail } from '@compos/use-crud'
// import { detailPrint } from '@/api/contract/sales-manage/price-manage/machine-part'
// import ExportButton from '@comp-common/export-button/index.vue'

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const { crud, detail, CRUD } = regDetail()

CRUD.HOOK.afterToDetail = () => {
  console.log(crud)
}

function handleClose() {
  crud.cancelDetail()
}
</script>
