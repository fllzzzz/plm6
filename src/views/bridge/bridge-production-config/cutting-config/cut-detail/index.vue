<template>
  <div>
    <div v-show="!cutConfigRow?.id">
      <div class="my-code">点击切割形式查看详情</div>
    </div>
    <div v-show="cutConfigRow?.id">
      <!--表格渲染-->
      <common-table ref="tableRef" :data="cutConfigRow.detailConfigDTOList" :max-height="maxHeight + 92" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="thickness" prop="thickness" :show-overflow-tooltip="true" align="center" label="厚度" width="140">
          <template #default="{ row }">
            <span> {{ row.thickness }}</span>
          </template>
        </el-table-column>
        <el-table-column key="boolDrillEnum" prop="boolDrillEnum" :show-overflow-tooltip="true" align="center" label="是否切孔" min-width="100">
          <template #default="{ row }">
            <el-switch
              v-model="row.boolDrillEnum"
              :active-value="whetherEnum.TRUE.V"
              :inactive-value="whetherEnum.FALSE.V"
              class="drawer-switch"
              @change="changeStatus(row, row.boolDrillEnum)"
            />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="130px" align="center" fixed="right">
          <template #default="{ row }">
            <common-button class="filter-item" size="mini" type="primary" icon="el-icon-edit" @click.stop="toEdit(row)" />
            <el-popover v-model:visible="row.deleteBtn" placement="top" width="180" trigger="manual">
              <p>确认删除本条数据吗？</p>
              <div style="text-align: right; margin: 0">
                <common-button size="mini" type="text" @click="cancelDelete">取消</common-button>
                <common-button type="primary" size="mini" @click="handleDelete">确定</common-button>
              </div>
              <template #reference>
                <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="toDelete(row)" />
              </template>
            </el-popover>
          </template>
        </el-table-column>
      </common-table>
      <m-form
        v-model:visible="formVisible"
        :isEdit="isEdit"
        :form-data="editRow"
        :cutConfigId="cutConfigRow.id"
        @refresh="refreshCutConfig"
      />
    </div>
  </div>
</template>

<script setup>
import { delCutConfigDetail as delApi, editCutConfigDetail as edit } from '@/api/mes/production-config/unloading-config'
import { ElMessageBox, ElNotification } from 'element-plus'
import { defineProps, defineExpose, ref, defineEmits, inject } from 'vue'
import { whetherEnum } from '@enum-ms/common'
import mForm from './module/form.vue'

const emit = defineEmits(['refresh-cut-config'])

const maxHeight = inject('maxHeight')

const isEdit = ref(false)
const formVisible = ref(false)
const editRow = ref({})

const props = defineProps({
  cutConfigRow: {
    type: Object,
    default: () => {}
  }
})

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm(`此操作将${props.cutConfigRow.name}的开孔方式改为${whetherEnum.VL[val]}, 是否继续？`, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await edit({ id: data.id, boolDrillEnum: val })
    refreshCutConfig()
    ElNotification({ title: `${props.cutConfigRow.name}的开孔方式修改成功`, type: 'success', duration: 3000 })
  } catch (error) {
    console.log('变更是否开孔状态', error)
    data.boolDrillEnum = data.boolDrillEnum === whetherEnum.TRUE.V ? whetherEnum.FALSE.V : whetherEnum.TRUE.V
  }
}

function toEdit(row) {
  editRow.value = row
  isEdit.value = true
  formVisible.value = true
}

function toDelete(row) {
  editRow.value = row
  row.deleteBtn = true
}

// 取消删除
function cancelDelete() {
  editRow.value.deleteBtn = false
}

// 刷新切割配置
function refreshCutConfig() {
  emit('refresh-cut-config')
}

// 确认删除
async function handleDelete() {
  editRow.value.deleteBtn = false
  try {
    await delApi([editRow.value.id])
    refreshCutConfig()
  } catch (error) {
    console.log('删除切割配置详情失败', error)
  }
}

function toAdd() {
  isEdit.value = false
  formVisible.value = true
}

defineExpose({
  toAdd
})
</script>
