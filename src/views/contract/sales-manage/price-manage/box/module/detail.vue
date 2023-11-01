<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    :visible="crud.detailVisible"
    :title="`详情`"
    :show-close="true"
    size="70%"
  >
    <template #titleAfter>
      <el-tag type="warning" size="medium" effect="plain">综合单价：<span>{{ props.detailInfo.originUnitPrice }}</span></el-tag>
    </template>
    <template #content>
      <common-table :data="list" row-key="rowId" v-loading="crud.detailLoading" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
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
import { ref, defineProps, defineEmits } from 'vue'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

const emit = defineEmits(['refresh'])

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({ extraBox: '.el-drawer__header', wrapperBox: '', extraHeight: 4 })

const list = ref([])
const { crud, detail, CRUD } = regDetail()

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  list.value = []
  try {
    list.value = (detail.content || []).map(v => {
      v.rowId = Math.random()
      return v
    })
  } catch (error) {
    crud.notify('获取分段详情', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 关闭
function handleClose() {
  crud.cancelDetail()
  emit('refresh')
}
</script>
