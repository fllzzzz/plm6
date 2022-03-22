<template>
  <span class="ud-operation" style="display: inline-block">
    <common-button @click="editClick(data)" v-if="data.state === '0' || data.state === '1'" type="warning" size="mini">修改</common-button>
    <el-popover
      v-if="data.state !== '2' && props.showDel && checkPermission(permission.del)"
      v-model:visible="pop"
      placement="top"
      width="180"
      trigger="manual"
      @show="onPopoverShow"
      @hide="onPopoverHide"
    >
      <p>{{ props.delPrompt }}</p>
      <div style="text-align: right; margin: 0">
        <common-button size="mini" type="text" @click="cancelDelete">取消</common-button>
        <common-button type="primary" size="mini" @click="handleDelete">确定</common-button>
      </div>
      <template #reference>
        <common-button
          :loading="crud.dataStatus[data.id].delete === 2"
          :disabled="props.disabledDel"
          type="danger"
          size="mini"
          @click.stop="toDelete"
        >
          撤销
        </common-button>
      </template>
    </el-popover>
    <common-button @click="NestingClick(data)" v-if="data.state === '0'" type="success" size="mini">去套料</common-button>
  </span>

  <!-- 零件工单 -->
  <common-dialog @close="closeDialog" width="70%" title="零件清单" append-to-body v-model="innerVisible">
    <common-table v-loading="innerLoading" ref="tableRef" :data="updateData" :max-height="400" style="width: 100%" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.monomerName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.totalNetWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作" min-width="60">
        <template v-slot="scope">
          <common-button v-if="scope.row.nestingState === 1" @click="delClick(scope.row)" type="danger" icon="el-icon-delete" size="mini" />
          <common-button v-else-if="scope.row.nestingState === 2" @click="addClick(scope.row)" type="primary" size="mini">
            新增
          </common-button>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, inject, defineEmits } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { regExtra } from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { del, uploadOrder, updateOrder, addPartFromOrder, deletePartFromOrder } from '@/api/cutting/taskPack'

const emit = defineEmits(['selectionChange'])

const props = defineProps({
  data: {
    type: Object,
    required: true
  },

  disabledDel: {
    type: Boolean,
    default: false
  },
  showDetail: {
    type: Boolean,
    default: false
  },
  showDel: {
    type: Boolean,
    default: true
  },

  delPrompt: {
    type: String,
    default: '选择撤销操作，让数据复原？'
  },
  beforeToDetail: {
    // 打开详情之前
    type: Function
  },
  beforeToEdit: {
    // 打开编辑之前
    type: Function
  }
})

const permission = inject('permission')
const innerLoading = ref(false)
const pop = ref(false)
const innerVisible = ref(false) // 弹出层
const updateData = ref([]) // 弹出层数据
const { crud } = regExtra()

// 取消删除
function cancelDelete() {
  pop.value = false
  crud.cancelDelete(props.data)
}

// 点击删除按钮
function toDelete() {
  pop.value = true
}

// 确认删除
async function handleDelete() {
  try {
    pop.value = false
    const message = await del(props.data.cutTaskId)
    ElMessage({ type: 'success', message: message })
    emit('query')
  } catch (err) {
    console.log(err)
  }
}

// 去套料？
async function NestingClick(row) {
  try {
    const message = await uploadOrder(props.data.cutTaskId)
    ElMessage({ type: 'success', message: message })
    emit('query')
  } catch (err) {
    console.log(err)
  }
}

// 编辑弹窗？？
async function editClick(row) {
  innerLoading.value = true
  innerVisible.value = true
  //   console.log('props.data.cutTaskId', props.data.cutTaskId)
  try {
    updateData.value = await updateOrder({ cutTaskId: props.data.cutTaskId })
    console.log('updateData', updateData.value)
  } catch (err) {
    console.log(err)
  }
  innerLoading.value = false
}

async function delClick(row) {
  try {
    const data = []
    data.push(row.id)
    const message = await deletePartFromOrder({ cutTaskId: props.data.cutTaskId }, data)
    ElMessage({ type: 'success', message: message })
    // 重新查询
    editClick()
  } catch (err) {
    console.log('addClick', err)
  }
}

async function addClick(row) {
  try {
    const data = []
    data.push(row.id)
    const message = await addPartFromOrder({ cutTaskId: props.data.cutTaskId }, data)
    ElMessage({ type: 'success', message: message })
    // 重新查询
    editClick()
  } catch (err) {
    console.log('addClick', err)
  }
}

function closeDialog() {
  emit('query')
}
// 打开删除提示窗
function onPopoverShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentClick, { passive: false })
  }, 0)
}

// 隐藏删除提示窗
function onPopoverHide() {
  document.removeEventListener('click', handleDocumentClick)
}

function handleDocumentClick(event) {
  pop.value = false
}

</script>

<style lang="scss" scoped>
::v-global(.ud-operation + .el-button) {
  margin-left: 7px;
}
::v-global(.el-button + .ud-operation) {
  margin-left: 7px;
}
.el-button + .el-button {
  margin-left: 7px;
}
</style>
