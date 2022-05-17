<template>
  <span class="ud-operation" style="display: inline-block">
    <!-- <common-button
      @click="editClick(data)"
      v-if="data.state === '0' || (data.state === '1' && checkPermission(permission.edit))"
      type="warning"
      size="mini"
      >修改</common-button
    > -->
    <el-popover
      v-if="data.state !== '2' && props.showDel && checkPermission(permission.revoke)"
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
          :disabled="props.disabledDel"
          type="danger"
          size="mini"
          @click.stop="toDelete"
        >
          撤销
        </common-button>
      </template>
    </el-popover>
          <!-- <el-popover
            v-if="data.state === '0'"
            v-model:visible="data.deleteBtn"
            placement="top"
            width="180"
            trigger="click"
            @show="onPopoverDelClickShow"
            @hide="onPopoverDelClickHide"
          >
            <p>选择删除操作，让数据删除？</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click="cancelDeleteBtn(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click="delClick(scope.row)">确定</common-button>
            </div>
            <template #reference>
              <common-button type="danger" size="mini" @click.stop="toDeleteBtn(scope.row)">删除</common-button>
            </template>
          </el-popover> -->
    <el-popover
      v-if="data.state === '0' && props.showDel && checkPermission(permission.Nesting)"
      v-model:visible="nesting"
      placement="top"
      width="180"
      trigger="manual"
      @show="onPopoverNestingShow"
      @hide="onPopoverNestingHide"
    >
      <p>选择套料操作，确认套料？</p>
      <div style="text-align: right; margin: 0">
        <common-button size="mini" type="text" @click="cancelNesting">取消</common-button>
        <common-button type="primary" size="mini" @click="NestingClick">确定</common-button>
      </div>
      <template #reference>
        <common-button type="success" size="mini" @click.stop="toNesting"> 去套料 </common-button>
      </template>
    </el-popover>

    <!-- <common-button @click="NestingClick(data)" v-if="data.state === '0'" type="success" size="mini">去套料</common-button> -->
  </span>

  <!-- 零件工单 -->
  <!-- <common-dialog @close="closeDialog" width="70%" title="零件清单" append-to-body v-model="innerVisible">
    <common-table v-loading="innerLoading" ref="tableRef" :data="updateData" :max-height="400" style="width: 100%" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.monomerName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="60" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.totalNetWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作" min-width="60">
        <template v-slot="scope"> -->
          <!-- <common-button v-if="scope.row.nestingState === 1" @click="delClick(scope.row)" type="danger" icon="el-icon-delete" size="mini" /> -->

          <!-- <el-popover
            v-if="scope.row.nestingState === 1"
            v-model:visible="scope.row.deleteBtn"
            placement="top"
            width="180"
            trigger="click"
            @show="onPopoverDelClickShow"
            @hide="onPopoverDelClickHide"
          >
            <p>选择删除操作，让数据删除？</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click="cancelDeleteBtn(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click="delClick(scope.row)">确定</common-button>
            </div>
            <template #reference>
              <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="toDeleteBtn(scope.row)" />
            </template>
          </el-popover>

          <el-popover
            v-else-if="scope.row.nestingState === 2"
            v-model:visible="scope.row.addBtn"
            placement="top"
            width="180"
            trigger="click"
            @show="onPopoverAddClickShow"
            @hide="onPopoverAddClickHide"
          >
            <p>选择新增操作，让数据添加？</p>
            <div style="text-align: right; margin: 0">
              <common-button size="mini" type="text" @click="cancelAddBtn(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click="addClick(scope.row)">确定</common-button>
            </div>
            <template #reference>
              <common-button type="primary" size="mini" @click.stop="toAddBtn(scope.row)"> 新增 </common-button>
            </template>
          </el-popover> -->
          <!--
          <common-button v-else-if="scope.row.nestingState === 2" @click="addClick(scope.row)" type="primary" size="mini">
            新增
          </common-button> -->
        <!-- </template>
      </el-table-column>
    </common-table>
  </common-dialog> -->
</template>

<script setup>
import { defineProps, ref, inject, defineEmits } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { regExtra } from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { del, uploadOrder } from '@/api/cutting/taskPack'
// import crudApi1 from '@/api/cutting/project-data'

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
// const innerLoading = ref(false)
const pop = ref(false)
// const innerVisible = ref(false) // 弹出层
// const updateData = ref([]) // 弹出层数据
const { crud } = regExtra()

const nesting = ref(false)

// 取消删除
function cancelDelete() {
  pop.value = false
  crud.cancelDelete(props.data)
}

// 取消套料
function cancelNesting() {
  nesting.value = false
}

// 点击套料按钮
function toNesting() {
  nesting.value = true
}

// 点击删除按钮
function toDelete() {
  pop.value = true
}

// function toDeleteBtn(row) {
//   row.deleteBtn = true
// }

// function toAddBtn(row) {
//   row.addBtn = true
// }

// function cancelDeleteBtn(row) {
//   row.deleteBtn = false
// }
// function cancelAddBtn(row) {
//   row.addBtn = false
// }

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
  nesting.value = false
}

// 编辑弹窗？？
// async function editClick(row) {
//   innerLoading.value = true
//   innerVisible.value = true
//   //   console.log('props.data.cutTaskId', props.data.cutTaskId)
//   try {
//     updateData.value = await updateOrder({ cutTaskId: props.data.cutTaskId })
//     updateData.value.forEach(v => {
//       v.deleteBtn = false
//       v.addBtn = false
//     })
//     console.log('updateData', updateData.value)
//   } catch (err) {
//     console.log(err)
//   }
//   innerLoading.value = false
// }

// async function delClick(row) {
//   try {
//     const data = []
//     data.push(row.id)
//     const message = await deletePartFromOrder({ cutTaskId: props.data.cutTaskId }, data)
//     ElMessage({ type: 'success', message: message })
//     // 重新查询
//     editClick()
//   } catch (err) {
//     console.log('addClick', err)
//   }
// }

// async function addClick(row) {
//   try {
//     const data = []
//     data.push(row.id)
//     const message = await addPartFromOrder({ cutTaskId: props.data.cutTaskId }, data)
//     ElMessage({ type: 'success', message: message })
//     // 重新查询
//     editClick()
//   } catch (err) {
//     console.log('addClick', err)
//   }
// }

// function closeDialog() {
//   emit('query')
// }
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

function handleDocumentNestingClick(event) {
  nesting.value = false
}

// 打开套料提示窗
function onPopoverNestingShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentNestingClick, { passive: false })
  }, 0)
}

// 隐藏套料提示窗
function onPopoverNestingHide() {
  document.removeEventListener('click', handleDocumentNestingClick)
}

// function handleDocumentDelClick(event) {
//   deleteBtn.value = false
// }

// function onPopoverDelClickShow() {
//   setTimeout(() => {
//     document.addEventListener('click', handleDocumentDelClick, { passive: false })
//   }, 0)
// }

// function onPopoverDelClickHide() {
//   document.removeEventListener('click', handleDocumentDelClick)
// }

// function handleDocumentAddClick(event) {
//   addBTn.value = false
// }

// function onPopoverAddClickShow() {
//   setTimeout(() => {
//     document.addEventListener('click', handleDocumentAddClick, { passive: false })
//   }, 0)
// }

// function onPopoverAddClickHide() {
//   document.removeEventListener('click', handleDocumentAddClick)
// }
// 请求钢板接口数据
// async function plateDataGet(row) {
//   currentRow.value = row
//   try {
//     const { content } = await crudApi1.get({ projectId: currentRow.value.projectId})
//     console.log('content', content)

//   } catch (err) {
//     console.log('钢板清单页面接口报错', err)
//   }
// }
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
