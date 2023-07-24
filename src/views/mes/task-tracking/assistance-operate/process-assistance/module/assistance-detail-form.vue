<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model:visible="detailVisible"
    :title="'工序协同'"
    :wrapper-closable="false"
    ref="drawerRef"
    size="90%"
  >
    <!-- <template #titleRight>
      <common-button type="primary" size="mini" @click="submit">确认</common-button>
    </template> -->
    <template #titleRight>
      <!-- <common-button size="mini" type="danger" @click="toBatchDelete" :disabled="!selections.length">批量删除【协同班组】</common-button> -->
      <common-button size="mini" type="primary" @click="submitIt">保存</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%; margin-top: 10px"
          class="upload-table"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
          @selection-change="handleSelectChange"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="100px" align="center" />
          <!-- <el-table-column
            v-if="crud.query.taskTypeEnum === taskTypeENUM.MACHINE_PART.V"
            prop="cutNumber"
            :show-overflow-tooltip="true"
            label="切割指令号"
            min-width="100px"
            align="center"
          />
          <el-table-column
            v-if="crud.query.taskTypeEnum === taskTypeENUM.ASSEMBLE.V"
            prop="attributeType"
            :show-overflow-tooltip="true"
            label="属性"
            width="90"
            align="center"
          >
            <template #default="{ row }">
              <el-tag :type="row.attributeType === '部件' ? 'warning' : 'success'">{{ row.attributeType }}</el-tag>
            </template>
          </el-table-column> -->
          <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100px" align="center" />
          <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="100px" align="center" />
          <el-table-column prop="length" :show-overflow-tooltip="true" label="长度(mm)" width="100px" align="center" />
          <!-- <el-table-column align="center" prop="totalTaskMete.quantity" :show-overflow-tooltip="true" label="任务数（件）">
            <template #default="{ row }">
              <span>{{ row.totalTaskMete?.quantity || 0 }}</span>
            </template>
          </el-table-column> -->
          <el-table-column align="center" prop="totalTaskMete.netWeight" :show-overflow-tooltip="true" label="任务总净重（kg）">
            <template #default="{ row }">
              <span>{{ row.totalTaskMete?.netWeight || 0 }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" prop="totalTaskMete.grossWeight" :show-overflow-tooltip="true" label="任务总毛重（kg）">
            <template #default="{ row }">
              <span>{{ row.totalTaskMete?.grossWeight || 0 }}</span>
            </template>
          </el-table-column>
          <!-- <el-table-column align="center" prop="totalCompleteMete.quantity" :show-overflow-tooltip="true" label="完成数（件）">
            <template #default="{ row }">
              <span>{{ row.totalCompleteMete?.quantity || 0 }}</span>
            </template>
          </el-table-column> -->
          <el-table-column align="center" prop="totalCompleteMete.netWeight" :show-overflow-tooltip="true" label="完成总净重（kg）">
            <template #default="{ row }">
              <span>{{ row.totalCompleteMete?.netWeight || 0 }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" prop="totalCompleteMete.grossWeight" :show-overflow-tooltip="true" label="完成总毛重（kg）">
            <template #default="{ row }">
              <span>{{ row.totalCompleteMete?.grossWeight || 0 }}</span>
            </template>
          </el-table-column>
          <!-- <el-table-column
          v-if="crud.query.taskTypeEnum === taskTypeENUM.MACHINE_PART.V"
          align="center"
          prop="askCompleteTime"
          :show-overflow-tooltip="true"
          label="计划完成日期"
          width="120px"
        /> -->
          <!-- <el-table-column
          v-if="crud.query.taskTypeEnum === taskTypeENUM.MACHINE_PART.V"
          prop="group.name"
          :show-overflow-tooltip="true"
          label="原生产组"
          min-width="180px"
        >
          <template #default="{ row }">
            <span>{{ row.workshop?.name }}>{{ row.productionLine?.name }}>{{ row.groups?.name }}</span>
          </template>
        </el-table-column> -->
          <el-table-column align="center" prop="quantity" :show-overflow-tooltip="true" label="可协同数量">
            <template #default="{ row }">
              <common-input-number
                v-model="row.quantity"
                :step="1"
                :controls="false"
                size="mini"
                placeholder="协同数量"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="groupsId" label="协同生产组" min-width="150px" align="center">
            <template #default="{ row }">
              <el-cascader
                v-model="row.groupsId"
                :ref="(el) => (cascaderRef[$index] = el)"
                :options="classIdGroupsObj[row.configId]?.list"
                :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
                show-all-levels
                :disabled="!row.boolUpdateGroup"
                filterable
                clearable
                placeholder="选择协同生产组"
                style="width: 100%"
                @expand-change="handleExpandChange($event, row, $index, cascaderRef[$index])"
                @focus="handleFocusChange($event, row, $index, cascaderRef[$index])"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="100" v-if="!form.id">
            <template v-slot="scope">
              <common-button
                size="small"
                :disabled="!scope.row.boolCanDelete"
                class="el-icon-delete"
                type="danger"
                @click="deleteRow(scope.$index)"
              />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-top: 15px"
            @click="addRow()"
            v-if="!form.id"
            >添加</common-button
          >
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import { save } from '@/api/mes/task-tracking/assistance-operate/process-assistance'
// import { shortTimeUploadFun } from '@/api/config/system-config/parallel-config'
// import { regForm } from '@compos/use-crud'
import useVisible from '@compos/use-visible'
import { ElNotification } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'
// import { DP } from '@/settings/config'

// import UploadBtn from './uploadBtn'

// const permission = inject('permission')

const cascaderRef = ref([])
const submitLoading = ref(false)
const selections = ref([])
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  detailList: {
    type: Array,
    default: () => []
  },
  classIdGroupsObj: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()
const form = ref({
  id: undefined,
  list: [],
  groupsId: undefined
})
const drawerRef = ref()

const { visible: detailVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

watch(
  () => detailVisible.value,
  (val) => {
    if (val) {
      form.value.list = []
      if (props.detailList.length) {
        props.detailList.forEach((v) => {
          const item = Object.assign(v, JSON.parse(JSON.stringify(props.detailData)))
          form.value.list.push(item)
        })
      }
    //   else {
    //     form.value.list.push(JSON.parse(JSON.stringify(props.detailData)))
    //   }
    }
  },
  { immediate: true, deep: true }
)

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body', '.upload-table'],
    navbar: false,
    extraHeight: 120
  },
  drawerRef.value
)

const validateGroupsId = (value, row) => {
  if (!value) return false
  return true
}

const validateQuantity = (value, row) => {
  if (!value) return false
  return true
}

const tableRules = {
  quantity: [{ validator: validateQuantity, required: true, message: '输入协同数量', trigger: 'blur' }],
  groupsId: [{ validator: validateGroupsId, required: true, message: '选择生产组', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function handleSelectChange(val) {
  selections.value = val
}

// --------------------------- 设置级联数据默认展开第一个【防止面板跳来跳去】 start ------------------------------

function handleFocusChange(expend, row, index, curCascaderRef) {
  if (!row.groupsId) {
    const menus = curCascaderRef.panel.menuList[0]
    setCascaderExpandNode(menus, menus.nodes[0])
  }
}

function setCascaderExpandNode(menus, node) {
  if (!node.isLeaf) {
    menus.panel.expandNode(node, true)
    if (!node.children[0].isLeaf) {
      setCascaderExpandNode(menus, node.children[0])
    }
  }
}

function handleExpandChange(expend, row, index, curCascaderRef) {
  const menus = curCascaderRef.panel.menuList[0]
  const curExpandingNode = menus.panel.expandingNode
  if (!curExpandingNode.children[0].isLeaf) {
    setCascaderExpandNode(menus, curExpandingNode.children[0])
  }
}

// --------------------------- 设置级联数据默认展开第一个【防止面板跳来跳去】 end --------------------------------

async function addRow() {
  const boolQuery = { boolUpdateGroup: true, boolCanDelete: true }
  const item = Object.assign(JSON.parse(JSON.stringify(props.detailData)), boolQuery)
  form.value.list.push(item)
}
function deleteRow(index) {
  form.value.list.splice(index, 1)
}

async function submitIt() {
  submitLoading.value = true
  const { validResult, dealList } = tableValidate(form.value.list)
  if (validResult) {
    form.value.list = dealList
  } else {
    return validResult
  }
  try {
    const _list = form.value.list?.map((v) => {
      return {
        id: v.id,
        quantity: v.quantity,
        groupsId: v.groupsId
      }
    })
    console.log(_list, '_list')
    await save({
      assistList: _list
    })
    ElNotification({
      title: '工序协同保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存工序协同报错', error)
  } finally {
    submitLoading.value = false
  }
}

// function toBatchDelete() {
//   if (!selections.value?.length) {
//     ElMessage.warning('请至少选择一条数据')
//     return
//   }
//   ElMessageBox.confirm(`是否确认删除所选择的班组协同？`, '提示', {
//     confirmButtonText: '确认',
//     cancelButtonText: '取消',
//     type: 'warning'
//   }).then(async () => {
//     try {
//       const ids = selections.value.map((v) => v.id)
//       await del({ ids })
//       ElNotification({ title: '班组删除成功', type: 'success' })
//       fetch()
//     } catch (error) {
//       console.log('班组删除失败', error)
//     }
//   })
// }
</script>
<style lang="scss" scoped>
::v-deep(.el-input__inner) {
  text-align: left;
  padding-left: 3px !important;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
.add-row-box {
  text-align: center;
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 3px;
    padding-right: 3px;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
</style>
