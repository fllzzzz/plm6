<template>
  <div class="app-container">
    <template v-if="currentProject && currentProject.projectContentList && currentProject.projectContentList.length>0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId"/>
      </div>
      <!--表格渲染-->
      <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      class="assembly-table"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="name" :show-overflow-tooltip="true" align="center" label="名称">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="plate" :show-overflow-tooltip="true" align="center" label="板型" width="120">
        <template v-slot="scope">
          <span>{{ scope.row.plate }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="thickness" :show-overflow-tooltip="true" align="center" label="厚度" width="120">
        <template v-slot="scope">
          <span>{{  scope.row.thickness? scope.row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="width" :show-overflow-tooltip="true" align="center" label="有效宽度" width="120">
        <template v-slot="scope">
          <span>{{ scope.row.width? scope.row.width.toFixed(DP.MES_ENCLOSURE_W__MM): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" label="钢板信息">
        <el-table-column align="center" label="内外板">
          <template>
            <div class="sandwich-cell-top">外板</div>
            <div class="sandwich-cell-bottom">内板</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="板形状">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.boardShape }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.boardShape }}</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="品牌">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.brand }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.brand }}</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="厚度">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.thickness? scope.row.outboard.thickness.toFixed(DP.MES_ENCLOSURE_T__MM): '-' }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.thickness? scope.row.inboard.thickness.toFixed(DP.MES_ENCLOSURE_T__MM): '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="宽度">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.width? scope.row.outboard.width.toFixed(DP.MES_ENCLOSURE_W__MM): '-' }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.width? scope.row.inboard.width.toFixed(DP.MES_ENCLOSURE_W__MM): '-'}}</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="镀层">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.claddingMaterial }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.claddingMaterial }}</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="涂层">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.coating }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.coating }}</div>
          </template>
        </el-table-column>
        <el-table-column align="center" label="颜色">
          <template v-slot="scope">
            <div class="sandwich-cell-top">{{ scope.row.outboard.color }}</div>
            <div class="sandwich-cell-bottom">{{ scope.row.inboard.color }}</div>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column align="center" label="芯材信息">
        <el-table-column prop="brand" :show-overflow-tooltip="true" align="center" label="品牌">
          <template v-slot="scope">
            <span>{{ scope.row.brand }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="type" :show-overflow-tooltip="true" align="center" label="种类">
          <template v-slot="scope">
            <span>{{ scope.row.type }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="capacity" :show-overflow-tooltip="true" align="center" label="容重">
          <template v-slot="scope">
            <span>{{ scope.row.capacity? scope.row.capacity.toFixed(2): '-' }}</span>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column prop="length" :show-overflow-tooltip="true" align="center" label="单长(㎜)">
        <template v-slot="scope">
          <span>{{ scope.row.length? scope.row.length.toFixed(DP.MES_ENCLOSURE_L__MM): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" align="center" label="数量(张)">
        <template v-slot="scope">
          <span>{{ scope.row.quantity? scope.row.quantity.toFixed(0): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalArea" :show-overflow-tooltip="true" align="center" label="总面积">
        <template v-slot="scope">
          <span>{{ scope.row.totalArea? scope.row.totalArea.toFixed(DP.COM_AREA__M2) : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalLength" :show-overflow-tooltip="true" align="center" label="总长度(m)">
        <template v-slot="scope">
          <span>{{ scope.row.totalLength? scope.row.totalLength.toFixed(DP.MES_ENCLOSURE_L__M): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="remark" :show-overflow-tooltip="true" align="center" label="备注">
        <template v-slot="scope">
          <span>{{ scope.row.remark }}</span>
        </template>
      </el-table-column>
      <!--状态、编辑与删除-->
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="70px" fixed="right">
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.boolStatusEnum"
            :disabled="!checkPermission(permission.edit)"
            active-color="#13ce66"
            :active-value="processingEnum.PROCESS.V"
            :inactive-value="processingEnum.PAUSE.V"
            @change="changeStatus(scope.row, scope.row.boolStatusEnum)"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.del])"
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation
            :data="scope.row"
          />
        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
    </template>
    <!-- <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template> -->
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/plan/technical-manage/enclosure'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { DP } from '@/settings/config'
import { processingEnum } from '@enum-ms/plan'
import mForm from './module/form'
import { ElMessageBox } from 'element-plus'

const { currentProject, globalProjectId } = mapGetters(['currentProject','globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['sandwich:get'],
  edit: ['sandwich:edit'],
  del: ['sandwich:del'],
  editStatus: ['sandwich:editStatus'],
  importList: ['sandwich:import']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const typeInfo = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '夹芯板清单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId'],
    crudApi: { ...crudApi },
    hasPagination: true
  }
)
const maxNubmer=99999999
const { maxHeight } = useMaxHeight({
  wrapperBox: '.sandwich',
  paginate: true,
  extraHeight: 157
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true }
)

async function changeStatus(data, val) {
  try {
    const messageName = val===1 ? '启用' : '暂停'
    await ElMessageBox.confirm('确定' + messageName + '?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.id)
    crud.notify(`“${data.name}”已【${messageName}】`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (error) {
    console.log('修改桁架楼层板状态', error)
    data.boolStatusEnum = data.boolStatusEnum===1?0:1
  }
}

CRUD.HOOK.handleRefresh= (crud, data) => {
  data.data.content = data.data.content.map(v => {
    if (v.sandwichColorBoardList && v.sandwichColorBoardList.length > 0) {
      v.sandwichColorBoardList.forEach(k => {
        if (k.type === 1) {
          v.inboard = k
        } else if (k.type === 2) {
          v.outboard = k
        }
      })
    }else{
      v.inboard = {}
      v.outboard = {}
    }
    return v
  })
}  

</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #ffecec;
}
.sandwich-cell-top{
  border-bottom: 1px solid #dfe6ec;
}
.sandwich-cell-top,.sandwich-cell-bottom{
  padding:5px;
  height:40px;
  line-height:30px;
  box-sizing:border-box;
  overflow:hidden;
}
.assembly-table{
  ::v-deep(.cell){
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th){
    background:#fff;
  }
}
</style>
