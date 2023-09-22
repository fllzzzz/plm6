<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    :visible="crud.detailVisible"
    :title="`${props.detailInfo?.name}详情`"
    :show-close="true"
    size="70%"
  >
    <template #titleAfter>
      <el-tag type="warning" size="medium" effect="plain">综合单价：<span>{{ props.detailInfo.originUnitPrice }}</span></el-tag>
    </template>
    <template #content>
      <common-table :data="list" row-key="rowId" v-loading="crud.detailLoading" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="name" label="名称" min-width="100px" align="center" />
        <el-table-column prop="serialNumber" label="编号" min-width="100px" align="center" />
        <el-table-column prop="specification" label="规格" min-width="100px" align="center" />
        <el-table-column prop="material" label="材质" min-width="100px" align="center" />
        <el-table-column prop="quantity" label="数量" min-width="80px" align="center" />
        <el-table-column prop="grossWeight" label="单重(kg)" min-width="100px" align="center" />
        <!--编辑-->
        <el-table-column v-if="checkPermission(crud.permission.edit) && list.length > 1" label="操作" width="70" align="center" fixed="right">
          <template #default="{ row }">
            <common-button type="primary" size="mini" icon="el-icon-edit" @click.stop="edit(row)" />
          </template>
        </el-table-column>
      </common-table>
      <!-- 调整 -->
      <adjust :detail="rowDetail" :structure-id="props.detailInfo.id" v-model="adjustVisible" @success="refreshData" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits } from 'vue'

import checkPermission from '@/utils/system/check-permission'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import adjust from './adjust'

const emit = defineEmits(['refresh'])

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({ extraBox: '.el-drawer__header', wrapperBox: '', extraHeight: 4 })

const list = ref([])
const rowDetail = ref({})
const adjustVisible = ref(false)
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
    crud.notify('获取构件详情', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 调整数据
function edit(row) {
  rowDetail.value = row
  adjustVisible.value = true
}

// 刷新页面
function refreshData() {
  crud.toDetail(props.detailInfo)
  emit('refresh')
}

// 关闭
function handleClose() {
  crud.cancelDetail()
}
</script>
