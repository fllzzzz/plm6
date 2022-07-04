<template>
  <div class="app-container">
    <template v-if="pageShow">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" :globalProject="globalProject"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        row-key="id"
        :row-class-name="handleAssemblyRowClassName"
        :cell-class-name="cellClassName"
        :expand-row-keys="expandArr"
        class="assembly-table"
        style="width: 100%"
        :stripe="false"
        return-source-data
        :showEmptySymbol="false"
        @selection-change="crud.selectionChangeHandler"
      >
        <el-table-column key="selection" type="selection" width="55" />
        <el-table-column type="expand">
          <template v-slot="scope">
            <div :key="`'singleTable${scope.row.id}'`">
              <common-table
                :data="scope.row.artifactDTOList"
                class="customer-table"
                :cell-class-name="wrongCellMask"
                row-key="rowKey"
                :stripe="false"
                style="width: 100%; border-color: transparent"
                return-source-data
                :showEmptySymbol="false"
              >
                <el-table-column key="serialNumber" prop="serialNumber" label="构件编号" align="center">
                  <template v-slot="scope">
                    <el-input
                      v-if="scope.row.add"
                      v-model="scope.row.serialNumber"
                      type="text"
                      placeholder="构件编号"
                      style="min-width: 100px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.serialNumber }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="specification" prop="specification" label="规格" align="center">
                  <template v-slot="scope">
                    <el-input
                      v-if="scope.row.add"
                      v-model="scope.row.specification"
                      type="text"
                      placeholder="规格"
                      style="min-width: 100px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.specification?scope.row.specification:'-' }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="material" prop="material" label="材质" align="center">
                  <template v-slot="scope">
                    <el-input
                      v-if="scope.row.add"
                      v-model="scope.row.material"
                      type="text"
                      placeholder="材质"
                      style="min-width: 100px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.material }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="length" prop="length" label="长度" align="center">
                  <template v-slot="scope">
                    <el-input-number
                      v-if="scope.row.add"
                      v-model.number="scope.row.length"
                      :min="0"
                      :max="maxNumber"
                      :step="1"
                      placeholder="请填写"
                      :precision="DP.MES_ARTIFACT_L__MM"
                      controls-position="right"
                      style="width: 90px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.length ? scope.row.length.toFixed(DP.MES_MACHINE_PART_L__MM) : '-' }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="netWeight" prop="netWeight" label="单净重" align="center">
                  <template v-slot="scope">
                    <el-input-number
                      v-if="scope.row.add"
                      v-model.number="scope.row.netWeight"
                      :min="0"
                      :max="maxNumber"
                      :step="1"
                      placeholder="请填写"
                      :precision="DP.COM_WT__KG"
                      controls-position="right"
                      style="width: 120px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="quantity" prop="quantity" label="数量" align="center">
                  <template v-slot="scope">
                    <el-input-number
                      v-if="scope.row.add"
                      v-model.number="scope.row.quantity"
                      :min="0"
                      :max="maxNumber"
                      :step="1"
                      step-strictly
                      placeholder="请填写"
                      controls-position="right"
                      style="width: 140px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.quantity?scope.row.quantity:'-' }}</span>
                  </template>
                </el-table-column>
                <el-table-column v-if="columns.visible('assembleQuantity')" prop="assembleQuantity" :show-overflow-tooltip="true" align="center" label="对应组立数量">
                  <template v-slot="scope">
                    <span v-if="!scope.row.add">{{ scope.row.assembleQuantity?scope.row.assembleQuantity:'-' }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="add" prop="add" label="已生产" align="center">
                  <template v-slot="scope">
                    <span v-if="!scope.row.add">{{ scope.row.productQuantity?scope.row.productQuantity:'-' }}</span>
                  </template>
                </el-table-column>
                <el-table-column label="操作" align="center">
                  <template v-slot="scope">
                    <common-button
                      v-if="scope.row.add"
                      type="primary"
                      size="mini"
                      plain
                      @click="addArtifact(scope.row)"
                      >保存</common-button>
                      <el-popconfirm title="确定删除吗?" @confirm="deleteRow(scope.row, scope.$index)" v-if="checkPermission(permission.artifactDel) || scope.row.add">
                        <template #reference>
                          <common-button type="danger" size="mini" plain>删除</common-button>
                        </template>
                      </el-popconfirm>
                  </template>
                </el-table-column>
              </common-table>
            </div>
          </template>
        </el-table-column>
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" :show-overflow-tooltip="true" align="center" label="组立号">
          <template #header>
            <el-tooltip class="item" effect="light" :content="`双击编号可预览图纸`" placement="top">
              <div style="display: inline-block">
                <span>组立号</span>
                <i class="el-icon-info" />
              </div>
            </el-tooltip>
          </template>
          <template v-slot="scope">
            <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('quantity')" prop="quantity" :show-overflow-tooltip="true" align="center" label="总数">
          <template v-slot="scope">
            <span>{{ scope.row.quantity?scope.row.quantity:'-' }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('producedQuantity')" prop="producedQuantity" :show-overflow-tooltip="true" align="center" label="已生产">
          <template v-slot="scope">
            <span>{{ scope.row.producedQuantity?scope.row.producedQuantity:'-' }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('usedQuantity')" prop="usedQuantity" :show-overflow-tooltip="true" align="center" label="已使用">
          <template v-slot="scope">
            <span>{{ scope.row.usedQuantity?scope.row.usedQuantity:'-' }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" label="翼板腹板信息">
          <template v-for="item in keyList" :key="item.key">
            <el-table-column align="center" :label="item.label" :prop="item.key">
              <template v-slot="scope">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <div v-for="(k,i) in scope.row.detailDTOList" :key="k.id">
                    <div :class="i===scope.row.detailDTOList.length-1?'sandwich-cell-bottom':'sandwich-cell-top'">
                      {{k[item.key]?k[item.key]:'-'}}
                    </div>
                  </div>
                </template>
                <template v-else>-</template>
              </template>
            </el-table-column>
          </template>
        </el-table-column>
        <el-table-column prop="remark" :show-overflow-tooltip="true" align="center" label="备注">
          <template v-slot="scope">
            <span>{{ scope.row.remark?scope.row.remark:'-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="checkPermission([...permission.del])"
          label="操作"
          width="150px"
          align="center"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" :show-edit="false" />
            <el-tooltip
              class="item"
              effect="dark"
              content="绑定构件"
              placement="top"
            >
              <common-button
                type="primary"
                icon="el-icon-plus"
                size="mini"
                v-permission="crud.permission.artifactAdd"
                @click="addRow(scope.row, scope.$index)"
                style="margin-left: 8px"
              />
            </el-tooltip>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <!-- pdf预览 -->
      <drawing-preview-fullscreen-dialog
        v-model="showDrawing"
        :bool-bim="drawingRow?.boolBim"
        :serial-number="drawingRow?.serialNumber"
        :productId="drawingRow?.productId"
        :productType="drawingRow?.productType"
      />
    </template>
    <template v-else>
      <span style="color:red;font-size:13px;">{{pageText}}</span>
    </template>
  </div>
</template>

<script setup>
import crudApi, { delAssemblyArtifact, addAssemblyArtifact } from '@/api/plan/technical-manage/assembly'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { DP } from '@/settings/config'
import { validate } from '@compos/form/use-table-validate'
import { ElMessage } from 'element-plus'
import { assemblyListPM as permission } from '@/page-permission/plan'
import useDrawing from '@compos/use-drawing'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'
import { TechnologyTypeAllEnum, projectModeEnum } from '@enum-ms/contract'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
const { showDrawing, drawingRow, drawingPreview } = useDrawing({ pidField: 'id', productTypeField: 'ASSEMBLE' })

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const maxNumber = 999999999
const pageShow = ref(false)
const pageText = ref()

const keyList = [
  { label: '编号', key: 'serialNumber' },
  { label: '规格', key: 'specification' },
  { label: '材质', key: 'material' },
  { label: '长度', key: 'length' },
  { label: '单净重(kg)', key: 'netWeight' },
  { label: '数量', key: 'quantity' },
  { label: '已使用', key: 'usedQuantity' }
]
const tableRules = {
  serialNumber: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  specification: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  quantity: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  length: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  material: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  netWeight: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }]
}
function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? (row.existStatus === 1 ? '' : 'abnormal-row') : 'mask-td'
}
const expandArr = ref([])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '组立清单',
    sort: ['id.asc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId', 'monomerId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.assembly',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => globalProject.value,
  (val) => {
    if (globalProject.value.projectContentList?.length > 0) {
      if (globalProject.value.projectContentList.findIndex(v => v.no === TechnologyTypeAllEnum.STRUCTURE.V) > -1) {
        pageShow.value = globalProject.value.mode !== projectModeEnum.STRUCTURE.V
        pageText.value = globalProject.value.mode !== projectModeEnum.STRUCTURE.V ? '当前项目内容没有包含构件,请到合同管理中进行配置' : '当前项目模式不包含组立'
      } else {
        pageText.value = '当前项目内容没有包含构件,请到合同管理中进行配置'
        pageShow.value = false
      }
    } else {
      pageText.value = '当前项目内容没有包含构件,请到合同管理中进行配置'
      pageShow.value = false
    }
  },
  { deep: true, immediate: true }
)
//

function handleAssemblyRowClassName({ row, rowIndex }) {
  return row.abnormal === 1 ? 'abnormal-row' : ''
}

function cellClassName({ row, rowIndex }) {
  return row.abnormal === 1 ? 'abnormal-row' : ''
}

function addRow(val, index) {
  if (expandArr.value.indexOf(val.id) < 0) {
    expandArr.value.push(val.id)
  }
  val.artifactDTOList.push({
    assembleId: val.id,
    length: undefined,
    material: '',
    netWeight: undefined,
    quantity: undefined,
    serialNumber: '',
    specification: '',
    mainIndex: index,
    existStatus: 1,
    add: true
  })
}
async function addArtifact(val) {
  const rules = tableRules
  let flag = true
  val.verify = {}
  for (const rule in rules) {
    val.verify[rule] = validate(rule, rules[rule], val)
    if (!val.verify[rule]) {
      flag = false
    }
  }
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return
  }
  try {
    await addAssemblyArtifact(val)
    crud.notify('添加成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('添加构件', e)
  }
}

async function deleteRow(val, index) {
  if (!val.add) {
    try {
      val.popoverVisible = false
      await delAssemblyArtifact({ artifactNo: val.serialNumber, assembleId: crud.data[val.mainIndex].id })
      crud.notify('操作成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
      crud.toQuery()
    } catch (e) {
      console.log('删除构件', e)
    }
  } else {
    crud.data[val.mainIndex].artifactDTOList.splice(index, 1)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v, index) => {
    v.mainEdit = false
    if (v.artifactDTOList && v.artifactDTOList.length > 0) {
      v.artifactDTOList.forEach((k) => {
        k.mainIndex = index
        k.popoverVisible = false
      })
    }
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  return !!crud.form.monomerId
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #ffecec;
}
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
    text-align: left;
  }
  &::before {
    width: 0;
  }
}
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 2px;
  }
}
.assembly-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
</style>
