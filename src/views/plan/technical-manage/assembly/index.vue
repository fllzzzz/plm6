<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        row-key="id"
        :expand-row-keys="expandArr"
        class="assembly-table"
        style="width: 100%"
      >
        <el-table-column type="expand">
          <template v-slot="scope">
            <div :key="`'singleTable${scope.row.id}'`">
              <common-table
                :data="scope.row.artifactDTOList"
                class="customer-table"
                :cell-class-name="wrongCellMask"
                :row-class-name="handleRowClassName"
                row-key="rowKey"
                :stripe="false"
                style="width: 100%; border-color: transparent"
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
                    <span v-else>{{ scope.row.specification }}</span>
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
                      :max="maxNubmer"
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
                      :max="maxNubmer"
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
                      :max="maxNubmer"
                      :step="1"
                      step-strictly
                      placeholder="请填写"
                      controls-position="right"
                      style="width: 140px"
                      size="mini"
                    />
                    <span v-else>{{ scope.row.quantity }}</span>
                  </template>
                </el-table-column>
                <el-table-column key="add" prop="add" label="已生产" align="center">
                  <template v-slot="scope">
                    <span v-if="!scope.row.add">{{ scope.row.productQuantity }}</span>
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
                    <common-button type="danger" size="mini" plain @click="deleteRow(scope.row, scope.$index)">删除</common-button>
                  </template>
                </el-table-column>
              </common-table>
            </div>
          </template>
        </el-table-column>
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('serialNumber')" prop="serialNumber" :show-overflow-tooltip="true" align="center" label="组立号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('quantity')" prop="quantity" :show-overflow-tooltip="true" align="center" label="总数">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('producedQuantity')" prop="producedQuantity" :show-overflow-tooltip="true" align="center" label="已生产">
          <template v-slot="scope">
            <span>{{ scope.row.producedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('usedQuantity')" prop="usedQuantity" :show-overflow-tooltip="true" align="center" label="已使用">
          <template v-slot="scope">
            <span>{{ scope.row.usedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" label="翼板腹板信息">
          <el-table-column align="center" label="编号" width="120">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input
                      v-model="scope.row.detailDTOList[0].serialNumber"
                      type="text"
                      placeholder="编号"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[0].serialNumber }}
                  </template>
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input
                      v-model="scope.row.detailDTOList[1].serialNumber"
                      type="text"
                      placeholder="编号"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[1].serialNumber }}
                  </template>
                </template>
              </div>
            </template>
          </el-table-column>
          <el-table-column align="center" label="规格" width="120">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input
                      v-model="scope.row.detailDTOList[0].specification"
                      type="text"
                      placeholder="规格"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[0].specification }}
                  </template>
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input
                      v-model="scope.row.detailDTOList[1].specification"
                      type="text"
                      placeholder="规格"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[1].specification }}
                  </template>
                </template>
              </div>
            </template>
          </el-table-column>
          <el-table-column align="center" label="材质" width="120">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input
                      v-model="scope.row.detailDTOList[0].material"
                      type="text"
                      placeholder="材质"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[0].material }}
                  </template>
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input
                      v-model="scope.row.detailDTOList[1].material"
                      type="text"
                      placeholder="材质"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[1].material }}
                  </template>
                </template>
              </div>
            </template>
          </el-table-column>
          <el-table-column align="center" label="长度" width="120">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input-number
                      v-model.number="scope.row.detailDTOList[0].length"
                      :min="0"
                      :max="maxNubmer"
                      :step="1"
                      placeholder="长度"
                      :precision="DP.MES_ARTIFACT_L__MM"
                      controls-position="right"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[0].length ? scope.row.detailDTOList[0].length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
                  </template>
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input-number
                      v-model.number="scope.row.detailDTOList[1].length"
                      :min="0"
                      :max="maxNubmer"
                      :step="1"
                      placeholder="长度"
                      :precision="DP.MES_ARTIFACT_L__MM"
                      controls-position="right"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[1].length ? scope.row.detailDTOList[1].length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
                  </template>
                </template>
              </div>
            </template>
          </el-table-column>
          <el-table-column align="center" label="重量" width="120">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input-number
                      v-model.number="scope.row.detailDTOList[0].netWeight"
                      :min="0"
                      :max="maxNubmer"
                      :step="1"
                      placeholder="重量"
                      :precision="DP.COM_WT__KG"
                      controls-position="right"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[0].netWeight ? scope.row.detailDTOList[0].netWeight.toFixed(DP.COM_WT__KG) : '-' }}
                  </template>
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  <template v-if="scope.row.mainEdit">
                    <el-input-number
                      v-model.number="scope.row.detailDTOList[1].netWeight"
                      :min="0"
                      :max="maxNubmer"
                      :step="1"
                      placeholder="重量"
                      :precision="DP.COM_WT__KG"
                      controls-position="right"
                      style="width: 100px"
                      size="mini"
                    />
                  </template>
                  <template v-else>
                    {{ scope.row.detailDTOList[1].netWeight ? scope.row.detailDTOList[1].netWeight.toFixed(DP.COM_WT__KG) : '-' }}
                  </template>
                </template>
              </div>
            </template>
          </el-table-column>
          <el-table-column align="center" label="数量">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  {{ scope.row.detailDTOList[0].quantity }}
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  {{ scope.row.detailDTOList[1].quantity }}
                </template>
              </div>
            </template>
          </el-table-column>
          <el-table-column align="center" label="已使用">
            <template v-slot="scope">
              <div class="sandwich-cell-top">
                <template v-if="scope.row.detailDTOList.length > 0">
                  {{ scope.row.detailDTOList[0].usedQuantity }}
                </template>
              </div>
              <div class="sandwich-cell-bottom">
                <template v-if="scope.row.detailDTOList.length > 0">
                  {{ scope.row.detailDTOList[1].usedQuantity }}
                </template>
              </div>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column prop="remark" :show-overflow-tooltip="true" align="center" label="备注">
          <template v-slot="scope">
            <span>{{ scope.row.remark }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="260px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <template v-if="scope.row.mainEdit">
              <common-button type="info" size="mini" plain @click="cancelMainRow(scope.row)">取消</common-button>
              <common-button type="primary" size="mini" @click="confirmRow(scope.row, scope.$index)">保存</common-button>
            </template>
            <template v-else>
              <common-button type="primary" icon="el-icon-edit" size="mini" @click="modifyRow(scope.row, scope.$index)" />
            </template>
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
                @click="addRow(scope.row, scope.$index)"
                style="margin-left: 8px"
              />
            </el-tooltip>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
    </template>
    <!-- <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template> -->
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
import useTableValidate from '@compos/form/use-table-validate'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['assembly:get'],
  edit: ['assembly:edit'],
  importList: ['assembly:import'],
  del: ['assembly:del'],
  addArtifact: ['assembly:addArtifact'],
  delArtifact: ['assembly:delArtifact']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const originRow = ref({})
const maxNubmer = 999999999
const tableRules = {
  serialNumber: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  specification: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  quantity: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  length: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  material: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  netWeight: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })
const expandArr = ref([])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '组立清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId', 'monomerId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function handleRowClassName({ row, rowIndex }) {
  return row.existStatus === 1 ? '' : 'abnormal-row'
}

function modifyRow(val) {
  originRow.value = JSON.parse(JSON.stringify(val))
  val.mainEdit = true
}

async function confirmRow(val) {
  val.mainEdit = false
  try {
    const editAssem = {
      id: val.id,
      detailUpdateDTOParams: val.detailDTOList
    }
    await crudApi.edit(editAssem)
  } catch (e) {
    val = Object.assign(val, originRow.value)
  } finally {
    crud.toQuery()
  }
}

function cancelMainRow(val) {
  val.mainEdit = false
  val = Object.assign(val, originRow.value)
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
  try {
    const { validResult, dealList } = tableValidate(crud.data[val.mainIndex].artifactDTOList)
    if (validResult) {
      crud.data[val.mainIndex].artifactDTOList = dealList
    } else {
      return validResult
    }
    await addAssemblyArtifact(val)
    crud.notify('添加成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log('添加构件', e)
  }
}

async function deleteRow(val, index) {
  if (val.id) {
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
  background: #fdf5e6;
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
