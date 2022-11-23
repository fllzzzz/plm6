<template>
  <common-drawer ref="drawerRef" title="无需套料清单" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #titleRight>
      <common-button type="danger" size="mini" icon="el-icon-delete" @click="batchDelete">批量移出</common-button>
    </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="tableData"
        :max-height="maxHeight"
        style="width: 100%"
        @selection-change="handleSelectChange"
      >
        <el-table-column type="selection" width="55" align="center" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" min-width="100px" align="center" />
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" min-width="100px" align="center" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="部件编号" width="120px" align="center" />
        <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" width="80px" />
        <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
        <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" align="center" width="100px" />
        <el-table-column prop="material" :show-overflow-tooltip="true" label="材质" align="center" width="100px" />
        <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" align="center" width="100px" />
        <el-table-column label="操作" align="center" width="100px">
          <template #default="{ row }">
            <popover-confirm @confirm="delRow(row)">
              <p>是否确认将该数据移出【无需套料清单】？</p>
              <template #reference>
                <common-button icon="el-icon-delete" type="danger" size="mini" />
              </template>
            </popover-confirm>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getNotNeedNesting, delNotNeedNesting } from '@/api/mes/craft-manage/section-steel/nesting-setting'
import { defineProps, defineEmits, ref, inject } from 'vue'
import { ElMessage, ElNotification, ElMessageBox } from 'element-plus'

import PopoverConfirm from '@/components-system/common/popover-confirm.vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const crud = inject('crud')
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableLoading = ref(false)
const tableData = ref([])
const selections = ref([])

function showHook() {
  fetch()
}

async function fetch() {
  try {
    tableLoading.value = true
    const { content } = await getNotNeedNesting(crud.query)
    tableData.value = content
  } catch (er) {
    console.log('获取无需套料清单失败', er)
  } finally {
    tableLoading.value = false
  }
}

function handleSelectChange(val) {
  selections.value = val
}

async function batchDelete() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }

  try {
    ElMessageBox.confirm(`是否确认将所选的数据移出【无需套料清单】`, '提示', {
      confirmButtonText: '确认',
      cancelButtonText: '取消',
      type: 'warning'
    }).then(async () => {
      try {
        const _ids = selections.value.map((v) => v.assembleDetailId)
        await delNotNeedNesting(_ids)
        ElNotification({
          title: '移出成功',
          type: 'success',
          duration: 2500
        })
        fetch()
        emit('refresh')
      } catch (error) {
        console.log('移出失败', error)
      }
    })
  } catch (er) {
    console.log('移出失败', er)
  }
}

async function delRow(row) {
  try {
    await delNotNeedNesting([row.assembleDetailId])
    ElNotification({
      title: '移出成功',
      type: 'success',
      duration: 2500
    })
    fetch()
    emit('refresh')
  } catch (er) {
    console.log('移出失败', er)
  }
}
</script>
